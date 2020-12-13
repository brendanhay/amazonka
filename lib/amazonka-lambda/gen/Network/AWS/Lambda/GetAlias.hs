{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
module Network.AWS.Lambda.GetAlias
  ( -- * Creating a request
    GetAlias (..),
    mkGetAlias,

    -- ** Request lenses
    gaName,
    gaFunctionName,

    -- * Destructuring the response
    AliasConfiguration (..),
    mkAliasConfiguration,

    -- ** Response lenses
    acRoutingConfig,
    acName,
    acFunctionVersion,
    acAliasARN,
    acDescription,
    acRevisionId,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAlias' smart constructor.
data GetAlias = GetAlias'
  { -- | The name of the alias.
    name :: Lude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @MyFunction@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:MyFunction@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAlias' with the minimum fields required to make a request.
--
-- * 'name' - The name of the alias.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
mkGetAlias ::
  -- | 'name'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  GetAlias
mkGetAlias pName_ pFunctionName_ =
  GetAlias' {name = pName_, functionName = pFunctionName_}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaName :: Lens.Lens' GetAlias Lude.Text
gaName = Lens.lens (name :: GetAlias -> Lude.Text) (\s a -> s {name = a} :: GetAlias)
{-# DEPRECATED gaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaFunctionName :: Lens.Lens' GetAlias Lude.Text
gaFunctionName = Lens.lens (functionName :: GetAlias -> Lude.Text) (\s a -> s {functionName = a} :: GetAlias)
{-# DEPRECATED gaFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetAlias where
  type Rs GetAlias = AliasConfiguration
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAlias where
  toPath GetAlias' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/aliases/",
        Lude.toBS name
      ]

instance Lude.ToQuery GetAlias where
  toQuery = Lude.const Lude.mempty
