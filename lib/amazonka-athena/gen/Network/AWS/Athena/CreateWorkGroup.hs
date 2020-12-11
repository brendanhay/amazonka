{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.CreateWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workgroup with the specified name.
module Network.AWS.Athena.CreateWorkGroup
  ( -- * Creating a request
    CreateWorkGroup (..),
    mkCreateWorkGroup,

    -- ** Request lenses
    cwgConfiguration,
    cwgDescription,
    cwgTags,
    cwgName,

    -- * Destructuring the response
    CreateWorkGroupResponse (..),
    mkCreateWorkGroupResponse,

    -- ** Response lenses
    cwgrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateWorkGroup' smart constructor.
data CreateWorkGroup = CreateWorkGroup'
  { configuration ::
      Lude.Maybe WorkGroupConfiguration,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkGroup' with the minimum fields required to make a request.
--
-- * 'configuration' - The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
-- * 'description' - The workgroup description.
-- * 'name' - The workgroup name.
-- * 'tags' - A list of comma separated tags to add to the workgroup that is created.
mkCreateWorkGroup ::
  -- | 'name'
  Lude.Text ->
  CreateWorkGroup
mkCreateWorkGroup pName_ =
  CreateWorkGroup'
    { configuration = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgConfiguration :: Lens.Lens' CreateWorkGroup (Lude.Maybe WorkGroupConfiguration)
cwgConfiguration = Lens.lens (configuration :: CreateWorkGroup -> Lude.Maybe WorkGroupConfiguration) (\s a -> s {configuration = a} :: CreateWorkGroup)
{-# DEPRECATED cwgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgDescription :: Lens.Lens' CreateWorkGroup (Lude.Maybe Lude.Text)
cwgDescription = Lens.lens (description :: CreateWorkGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateWorkGroup)
{-# DEPRECATED cwgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of comma separated tags to add to the workgroup that is created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgTags :: Lens.Lens' CreateWorkGroup (Lude.Maybe [Tag])
cwgTags = Lens.lens (tags :: CreateWorkGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateWorkGroup)
{-# DEPRECATED cwgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The workgroup name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgName :: Lens.Lens' CreateWorkGroup Lude.Text
cwgName = Lens.lens (name :: CreateWorkGroup -> Lude.Text) (\s a -> s {name = a} :: CreateWorkGroup)
{-# DEPRECATED cwgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateWorkGroup where
  type Rs CreateWorkGroup = CreateWorkGroupResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateWorkGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.CreateWorkGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkGroup where
  toJSON CreateWorkGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Configuration" Lude..=) Lude.<$> configuration,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateWorkGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkGroupResponse' smart constructor.
newtype CreateWorkGroupResponse = CreateWorkGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateWorkGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkGroupResponse
mkCreateWorkGroupResponse pResponseStatus_ =
  CreateWorkGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgrsResponseStatus :: Lens.Lens' CreateWorkGroupResponse Lude.Int
cwgrsResponseStatus = Lens.lens (responseStatus :: CreateWorkGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkGroupResponse)
{-# DEPRECATED cwgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
