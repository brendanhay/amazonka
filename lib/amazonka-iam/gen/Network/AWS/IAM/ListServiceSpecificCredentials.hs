{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListServiceSpecificCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the service-specific credentials associated with the specified IAM user. If none exists, the operation returns an empty list. The service-specific credentials returned by this operation are used only for authenticating the IAM user to a specific service. For more information about using service-specific credentials to authenticate to an AWS service, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html Set Up service-specific credentials> in the AWS CodeCommit User Guide.
module Network.AWS.IAM.ListServiceSpecificCredentials
  ( -- * Creating a request
    ListServiceSpecificCredentials (..),
    mkListServiceSpecificCredentials,

    -- ** Request lenses
    lsscUserName,
    lsscServiceName,

    -- * Destructuring the response
    ListServiceSpecificCredentialsResponse (..),
    mkListServiceSpecificCredentialsResponse,

    -- ** Response lenses
    lsscrsServiceSpecificCredentials,
    lsscrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListServiceSpecificCredentials' smart constructor.
data ListServiceSpecificCredentials = ListServiceSpecificCredentials'
  { userName ::
      Lude.Maybe Lude.Text,
    serviceName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceSpecificCredentials' with the minimum fields required to make a request.
--
-- * 'serviceName' - Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
-- * 'userName' - The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkListServiceSpecificCredentials ::
  ListServiceSpecificCredentials
mkListServiceSpecificCredentials =
  ListServiceSpecificCredentials'
    { userName = Lude.Nothing,
      serviceName = Lude.Nothing
    }

-- | The name of the user whose service-specific credentials you want information about. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscUserName :: Lens.Lens' ListServiceSpecificCredentials (Lude.Maybe Lude.Text)
lsscUserName = Lens.lens (userName :: ListServiceSpecificCredentials -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ListServiceSpecificCredentials)
{-# DEPRECATED lsscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Filters the returned results to only those for the specified AWS service. If not specified, then AWS returns service-specific credentials for all services.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscServiceName :: Lens.Lens' ListServiceSpecificCredentials (Lude.Maybe Lude.Text)
lsscServiceName = Lens.lens (serviceName :: ListServiceSpecificCredentials -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ListServiceSpecificCredentials)
{-# DEPRECATED lsscServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest ListServiceSpecificCredentials where
  type
    Rs ListServiceSpecificCredentials =
      ListServiceSpecificCredentialsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListServiceSpecificCredentialsResult"
      ( \s h x ->
          ListServiceSpecificCredentialsResponse'
            Lude.<$> ( x Lude..@? "ServiceSpecificCredentials" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServiceSpecificCredentials where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListServiceSpecificCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServiceSpecificCredentials where
  toQuery ListServiceSpecificCredentials' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListServiceSpecificCredentials" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "ServiceName" Lude.=: serviceName
      ]

-- | /See:/ 'mkListServiceSpecificCredentialsResponse' smart constructor.
data ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse'
  { serviceSpecificCredentials ::
      Lude.Maybe
        [ServiceSpecificCredentialMetadata],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServiceSpecificCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serviceSpecificCredentials' - A list of structures that each contain details about a service-specific credential.
mkListServiceSpecificCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServiceSpecificCredentialsResponse
mkListServiceSpecificCredentialsResponse pResponseStatus_ =
  ListServiceSpecificCredentialsResponse'
    { serviceSpecificCredentials =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of structures that each contain details about a service-specific credential.
--
-- /Note:/ Consider using 'serviceSpecificCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrsServiceSpecificCredentials :: Lens.Lens' ListServiceSpecificCredentialsResponse (Lude.Maybe [ServiceSpecificCredentialMetadata])
lsscrsServiceSpecificCredentials = Lens.lens (serviceSpecificCredentials :: ListServiceSpecificCredentialsResponse -> Lude.Maybe [ServiceSpecificCredentialMetadata]) (\s a -> s {serviceSpecificCredentials = a} :: ListServiceSpecificCredentialsResponse)
{-# DEPRECATED lsscrsServiceSpecificCredentials "Use generic-lens or generic-optics with 'serviceSpecificCredentials' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrsResponseStatus :: Lens.Lens' ListServiceSpecificCredentialsResponse Lude.Int
lsscrsResponseStatus = Lens.lens (responseStatus :: ListServiceSpecificCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServiceSpecificCredentialsResponse)
{-# DEPRECATED lsscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
