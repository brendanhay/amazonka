{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListImports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see 'ListExports' .
--
-- For more information about importing an exported output value, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html @Fn::ImportValue@ > function.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListImports
  ( -- * Creating a request
    ListImports (..),
    mkListImports,

    -- ** Request lenses
    liNextToken,
    liExportName,

    -- * Destructuring the response
    ListImportsResponse (..),
    mkListImportsResponse,

    -- ** Response lenses
    lirsImports,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListImports' smart constructor.
data ListImports = ListImports'
  { -- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
    exportName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImports' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
-- * 'exportName' - The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
mkListImports ::
  -- | 'exportName'
  Lude.Text ->
  ListImports
mkListImports pExportName_ =
  ListImports' {nextToken = Lude.Nothing, exportName = pExportName_}

-- | A string (provided by the 'ListImports' response output) that identifies the next page of stacks that are importing the specified exported output value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListImports (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListImports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImports)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the exported output value. AWS CloudFormation returns the stack names that are importing this value.
--
-- /Note:/ Consider using 'exportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liExportName :: Lens.Lens' ListImports Lude.Text
liExportName = Lens.lens (exportName :: ListImports -> Lude.Text) (\s a -> s {exportName = a} :: ListImports)
{-# DEPRECATED liExportName "Use generic-lens or generic-optics with 'exportName' instead." #-}

instance Page.AWSPager ListImports where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsImports) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListImports where
  type Rs ListImports = ListImportsResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "ListImportsResult"
      ( \s h x ->
          ListImportsResponse'
            Lude.<$> ( x Lude..@? "Imports" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListImports where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListImports where
  toPath = Lude.const "/"

instance Lude.ToQuery ListImports where
  toQuery ListImports' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListImports" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "ExportName" Lude.=: exportName
      ]

-- | /See:/ 'mkListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | A list of stack names that are importing the specified exported output value.
    imports :: Lude.Maybe [Lude.Text],
    -- | A string that identifies the next page of exports. If there is no additional page, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListImportsResponse' with the minimum fields required to make a request.
--
-- * 'imports' - A list of stack names that are importing the specified exported output value.
-- * 'nextToken' - A string that identifies the next page of exports. If there is no additional page, this value is null.
-- * 'responseStatus' - The response status code.
mkListImportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListImportsResponse
mkListImportsResponse pResponseStatus_ =
  ListImportsResponse'
    { imports = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of stack names that are importing the specified exported output value.
--
-- /Note:/ Consider using 'imports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsImports :: Lens.Lens' ListImportsResponse (Lude.Maybe [Lude.Text])
lirsImports = Lens.lens (imports :: ListImportsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {imports = a} :: ListImportsResponse)
{-# DEPRECATED lirsImports "Use generic-lens or generic-optics with 'imports' instead." #-}

-- | A string that identifies the next page of exports. If there is no additional page, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListImportsResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListImportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListImportsResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListImportsResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListImportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListImportsResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
