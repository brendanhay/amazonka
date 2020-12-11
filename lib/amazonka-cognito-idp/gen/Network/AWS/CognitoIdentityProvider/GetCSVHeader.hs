{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetCSVHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the header information for the .csv file to be used as input for the user import job.
module Network.AWS.CognitoIdentityProvider.GetCSVHeader
  ( -- * Creating a request
    GetCSVHeader (..),
    mkGetCSVHeader,

    -- ** Request lenses
    gchUserPoolId,

    -- * Destructuring the response
    GetCSVHeaderResponse (..),
    mkGetCSVHeaderResponse,

    -- ** Response lenses
    gchrsUserPoolId,
    gchrsCSVHeader,
    gchrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get the header information for the .csv file for the user import job.
--
-- /See:/ 'mkGetCSVHeader' smart constructor.
newtype GetCSVHeader = GetCSVHeader' {userPoolId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCSVHeader' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool that the users are to be imported into.
mkGetCSVHeader ::
  -- | 'userPoolId'
  Lude.Text ->
  GetCSVHeader
mkGetCSVHeader pUserPoolId_ =
  GetCSVHeader' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool that the users are to be imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gchUserPoolId :: Lens.Lens' GetCSVHeader Lude.Text
gchUserPoolId = Lens.lens (userPoolId :: GetCSVHeader -> Lude.Text) (\s a -> s {userPoolId = a} :: GetCSVHeader)
{-# DEPRECATED gchUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest GetCSVHeader where
  type Rs GetCSVHeader = GetCSVHeaderResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCSVHeaderResponse'
            Lude.<$> (x Lude..?> "UserPoolId")
            Lude.<*> (x Lude..?> "CSVHeader" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCSVHeader where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetCSVHeader" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCSVHeader where
  toJSON GetCSVHeader' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("UserPoolId" Lude..= userPoolId)])

instance Lude.ToPath GetCSVHeader where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCSVHeader where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to get the header information for the .csv file for the user import job.
--
-- /See:/ 'mkGetCSVHeaderResponse' smart constructor.
data GetCSVHeaderResponse = GetCSVHeaderResponse'
  { userPoolId ::
      Lude.Maybe Lude.Text,
    csvHeader :: Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCSVHeaderResponse' with the minimum fields required to make a request.
--
-- * 'csvHeader' - The header information for the .csv file for the user import job.
-- * 'responseStatus' - The response status code.
-- * 'userPoolId' - The user pool ID for the user pool that the users are to be imported into.
mkGetCSVHeaderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCSVHeaderResponse
mkGetCSVHeaderResponse pResponseStatus_ =
  GetCSVHeaderResponse'
    { userPoolId = Lude.Nothing,
      csvHeader = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user pool ID for the user pool that the users are to be imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gchrsUserPoolId :: Lens.Lens' GetCSVHeaderResponse (Lude.Maybe Lude.Text)
gchrsUserPoolId = Lens.lens (userPoolId :: GetCSVHeaderResponse -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: GetCSVHeaderResponse)
{-# DEPRECATED gchrsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The header information for the .csv file for the user import job.
--
-- /Note:/ Consider using 'csvHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gchrsCSVHeader :: Lens.Lens' GetCSVHeaderResponse (Lude.Maybe [Lude.Text])
gchrsCSVHeader = Lens.lens (csvHeader :: GetCSVHeaderResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {csvHeader = a} :: GetCSVHeaderResponse)
{-# DEPRECATED gchrsCSVHeader "Use generic-lens or generic-optics with 'csvHeader' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gchrsResponseStatus :: Lens.Lens' GetCSVHeaderResponse Lude.Int
gchrsResponseStatus = Lens.lens (responseStatus :: GetCSVHeaderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCSVHeaderResponse)
{-# DEPRECATED gchrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
