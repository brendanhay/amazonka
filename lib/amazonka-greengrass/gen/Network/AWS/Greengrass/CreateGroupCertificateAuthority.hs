{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateGroupCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CA for the group. If a CA already exists, it will rotate the existing CA.
module Network.AWS.Greengrass.CreateGroupCertificateAuthority
  ( -- * Creating a request
    CreateGroupCertificateAuthority (..),
    mkCreateGroupCertificateAuthority,

    -- ** Request lenses
    cgcaAmznClientToken,
    cgcaGroupId,

    -- * Destructuring the response
    CreateGroupCertificateAuthorityResponse (..),
    mkCreateGroupCertificateAuthorityResponse,

    -- ** Response lenses
    cgcarsGroupCertificateAuthorityARN,
    cgcarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroupCertificateAuthority' smart constructor.
data CreateGroupCertificateAuthority = CreateGroupCertificateAuthority'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'groupId' - The ID of the Greengrass group.
mkCreateGroupCertificateAuthority ::
  -- | 'groupId'
  Lude.Text ->
  CreateGroupCertificateAuthority
mkCreateGroupCertificateAuthority pGroupId_ =
  CreateGroupCertificateAuthority'
    { amznClientToken = Lude.Nothing,
      groupId = pGroupId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaAmznClientToken :: Lens.Lens' CreateGroupCertificateAuthority (Lude.Maybe Lude.Text)
cgcaAmznClientToken = Lens.lens (amznClientToken :: CreateGroupCertificateAuthority -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateGroupCertificateAuthority)
{-# DEPRECATED cgcaAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaGroupId :: Lens.Lens' CreateGroupCertificateAuthority Lude.Text
cgcaGroupId = Lens.lens (groupId :: CreateGroupCertificateAuthority -> Lude.Text) (\s a -> s {groupId = a} :: CreateGroupCertificateAuthority)
{-# DEPRECATED cgcaGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest CreateGroupCertificateAuthority where
  type
    Rs CreateGroupCertificateAuthority =
      CreateGroupCertificateAuthorityResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupCertificateAuthorityResponse'
            Lude.<$> (x Lude..?> "GroupCertificateAuthorityArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroupCertificateAuthority where
  toHeaders CreateGroupCertificateAuthority' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateGroupCertificateAuthority where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateGroupCertificateAuthority where
  toPath CreateGroupCertificateAuthority' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/certificateauthorities"
      ]

instance Lude.ToQuery CreateGroupCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupCertificateAuthorityResponse' smart constructor.
data CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse'
  { -- | The ARN of the group certificate authority.
    groupCertificateAuthorityARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- * 'groupCertificateAuthorityARN' - The ARN of the group certificate authority.
-- * 'responseStatus' - The response status code.
mkCreateGroupCertificateAuthorityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupCertificateAuthorityResponse
mkCreateGroupCertificateAuthorityResponse pResponseStatus_ =
  CreateGroupCertificateAuthorityResponse'
    { groupCertificateAuthorityARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the group certificate authority.
--
-- /Note:/ Consider using 'groupCertificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarsGroupCertificateAuthorityARN :: Lens.Lens' CreateGroupCertificateAuthorityResponse (Lude.Maybe Lude.Text)
cgcarsGroupCertificateAuthorityARN = Lens.lens (groupCertificateAuthorityARN :: CreateGroupCertificateAuthorityResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupCertificateAuthorityARN = a} :: CreateGroupCertificateAuthorityResponse)
{-# DEPRECATED cgcarsGroupCertificateAuthorityARN "Use generic-lens or generic-optics with 'groupCertificateAuthorityARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarsResponseStatus :: Lens.Lens' CreateGroupCertificateAuthorityResponse Lude.Int
cgcarsResponseStatus = Lens.lens (responseStatus :: CreateGroupCertificateAuthorityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupCertificateAuthorityResponse)
{-# DEPRECATED cgcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
