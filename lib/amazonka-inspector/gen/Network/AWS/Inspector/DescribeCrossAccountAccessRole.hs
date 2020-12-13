{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dcaarrsRegisteredAt,
    dcaarrsValid,
    dcaarrsRoleARN,
    dcaarrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
            Lude.<$> (x Lude..:> "registeredAt")
            Lude.<*> (x Lude..:> "valid")
            Lude.<*> (x Lude..:> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The date when the cross-account access role was registered.
    registeredAt :: Lude.Timestamp,
    -- | A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
    valid :: Lude.Bool,
    -- | The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
    roleARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
-- * 'registeredAt' - The date when the cross-account access role was registered.
-- * 'valid' - A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
-- * 'roleARN' - The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
-- * 'responseStatus' - The response status code.
mkDescribeCrossAccountAccessRoleResponse ::
  -- | 'registeredAt'
  Lude.Timestamp ->
  -- | 'valid'
  Lude.Bool ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCrossAccountAccessRoleResponse
mkDescribeCrossAccountAccessRoleResponse
  pRegisteredAt_
  pValid_
  pRoleARN_
  pResponseStatus_ =
    DescribeCrossAccountAccessRoleResponse'
      { registeredAt =
          pRegisteredAt_,
        valid = pValid_,
        roleARN = pRoleARN_,
        responseStatus = pResponseStatus_
      }

-- | The date when the cross-account access role was registered.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsRegisteredAt :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Timestamp
dcaarrsRegisteredAt = Lens.lens (registeredAt :: DescribeCrossAccountAccessRoleResponse -> Lude.Timestamp) (\s a -> s {registeredAt = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsRegisteredAt "Use generic-lens or generic-optics with 'registeredAt' instead." #-}

-- | A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsValid :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Bool
dcaarrsValid = Lens.lens (valid :: DescribeCrossAccountAccessRoleResponse -> Lude.Bool) (\s a -> s {valid = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsRoleARN :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Text
dcaarrsRoleARN = Lens.lens (roleARN :: DescribeCrossAccountAccessRoleResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsResponseStatus :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Lude.Int
dcaarrsResponseStatus = Lens.lens (responseStatus :: DescribeCrossAccountAccessRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCrossAccountAccessRoleResponse)
{-# DEPRECATED dcaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
