{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeCrossAccountAccessRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IAM role that enables Amazon Inspector to access your AWS account.
module Network.AWS.Inspector.DescribeCrossAccountAccessRole
  ( -- * Creating a request
    DescribeCrossAccountAccessRole (..),
    mkDescribeCrossAccountAccessRole,

    -- * Destructuring the response
    DescribeCrossAccountAccessRoleResponse (..),
    mkDescribeCrossAccountAccessRoleResponse,

    -- ** Response lenses
    dcaarrsResponseStatus,
    dcaarrsRoleARN,
    dcaarrsValid,
    dcaarrsRegisteredAt,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCrossAccountAccessRole' with the minimum fields required to make a request.
mkDescribeCrossAccountAccessRole ::
  DescribeCrossAccountAccessRole
mkDescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'

instance Lude.AWSRequest DescribeCrossAccountAccessRole where
  type
    Rs DescribeCrossAccountAccessRole =
      DescribeCrossAccountAccessRoleResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCrossAccountAccessRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "roleArn")
            Lude.<*> (x Lude..:> "valid")
            Lude.<*> (x Lude..:> "registeredAt")
      )

instance Lude.ToHeaders DescribeCrossAccountAccessRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "InspectorService.DescribeCrossAccountAccessRole" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCrossAccountAccessRole where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeCrossAccountAccessRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCrossAccountAccessRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCrossAccountAccessRoleResponse' smart constructor.
data DescribeCrossAccountAccessRoleResponse = DescribeCrossAccountAccessRoleResponse'
  { responseStatus ::
      Lude.Int,
    roleARN ::
      Lude.Text,
    valid ::
      Lude.Bool,
    registeredAt ::
      Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
-- * 'registeredAt' - The date when the cross-account access role was registered.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
-- * 'valid' - A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
mkDescribeCrossAccountAccessRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'valid'
  Lude.Bool ->
  -- | 'registeredAt'
  Lude.Timestamp ->
  DescribeCrossAccountAccessRoleResponse
mkDescribeCrossAccountAccessRoleResponse
  pResponseStatus_
  pRoleARN_
  pValid_
  pRegisteredAt_ =
    DescribeCrossAccountAccessRoleResponse'
      { responseStatus =
          pResponseStatus_,
        roleARN = pRoleARN_,
        valid = pValid_,
        registeredAt = pRegisteredAt_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsResponseStatus :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Int
dcaarrsResponseStatus = Lens.lens (responseStatus :: DescribeCrossAccountAccessRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsRoleARN :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Text
dcaarrsRoleARN = Lens.lens (roleARN :: DescribeCrossAccountAccessRoleResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsValid :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Bool
dcaarrsValid = Lens.lens (valid :: DescribeCrossAccountAccessRoleResponse -> Lude.Bool) (\s a -> s {valid = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The date when the cross-account access role was registered.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsRegisteredAt :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Timestamp
dcaarrsRegisteredAt = Lens.lens (registeredAt :: DescribeCrossAccountAccessRoleResponse -> Lude.Timestamp) (\s a -> s {registeredAt = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsRegisteredAt "Use generic-lens or generic-optics with 'registeredAt' instead." #-}
