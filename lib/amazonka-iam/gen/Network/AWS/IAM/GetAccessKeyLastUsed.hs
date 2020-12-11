{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used. The information includes the date and time of last use, along with the AWS service and Region that were specified in the last request made with that key.
module Network.AWS.IAM.GetAccessKeyLastUsed
  ( -- * Creating a request
    GetAccessKeyLastUsed (..),
    mkGetAccessKeyLastUsed,

    -- ** Request lenses
    gakluAccessKeyId,

    -- * Destructuring the response
    GetAccessKeyLastUsedResponse (..),
    mkGetAccessKeyLastUsedResponse,

    -- ** Response lenses
    gaklursUserName,
    gaklursAccessKeyLastUsed,
    gaklursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccessKeyLastUsed' smart constructor.
newtype GetAccessKeyLastUsed = GetAccessKeyLastUsed'
  { accessKeyId ::
      AccessKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccessKeyLastUsed' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The identifier of an access key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
mkGetAccessKeyLastUsed ::
  -- | 'accessKeyId'
  AccessKey ->
  GetAccessKeyLastUsed
mkGetAccessKeyLastUsed pAccessKeyId_ =
  GetAccessKeyLastUsed' {accessKeyId = pAccessKeyId_}

-- | The identifier of an access key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakluAccessKeyId :: Lens.Lens' GetAccessKeyLastUsed AccessKey
gakluAccessKeyId = Lens.lens (accessKeyId :: GetAccessKeyLastUsed -> AccessKey) (\s a -> s {accessKeyId = a} :: GetAccessKeyLastUsed)
{-# DEPRECATED gakluAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.AWSRequest GetAccessKeyLastUsed where
  type Rs GetAccessKeyLastUsed = GetAccessKeyLastUsedResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetAccessKeyLastUsedResult"
      ( \s h x ->
          GetAccessKeyLastUsedResponse'
            Lude.<$> (x Lude..@? "UserName")
            Lude.<*> (x Lude..@? "AccessKeyLastUsed")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccessKeyLastUsed where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccessKeyLastUsed where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccessKeyLastUsed where
  toQuery GetAccessKeyLastUsed' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetAccessKeyLastUsed" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "AccessKeyId" Lude.=: accessKeyId
      ]

-- | Contains the response to a successful 'GetAccessKeyLastUsed' request. It is also returned as a member of the 'AccessKeyMetaData' structure returned by the 'ListAccessKeys' action.
--
-- /See:/ 'mkGetAccessKeyLastUsedResponse' smart constructor.
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
  { userName ::
      Lude.Maybe Lude.Text,
    accessKeyLastUsed ::
      Lude.Maybe AccessKeyLastUsed,
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

-- | Creates a value of 'GetAccessKeyLastUsedResponse' with the minimum fields required to make a request.
--
-- * 'accessKeyLastUsed' - Contains information about the last time the access key was used.
-- * 'responseStatus' - The response status code.
-- * 'userName' - The name of the AWS IAM user that owns this access key.
mkGetAccessKeyLastUsedResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccessKeyLastUsedResponse
mkGetAccessKeyLastUsedResponse pResponseStatus_ =
  GetAccessKeyLastUsedResponse'
    { userName = Lude.Nothing,
      accessKeyLastUsed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the AWS IAM user that owns this access key.
--
--
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklursUserName :: Lens.Lens' GetAccessKeyLastUsedResponse (Lude.Maybe Lude.Text)
gaklursUserName = Lens.lens (userName :: GetAccessKeyLastUsedResponse -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: GetAccessKeyLastUsedResponse)
{-# DEPRECATED gaklursUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Contains information about the last time the access key was used.
--
-- /Note:/ Consider using 'accessKeyLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklursAccessKeyLastUsed :: Lens.Lens' GetAccessKeyLastUsedResponse (Lude.Maybe AccessKeyLastUsed)
gaklursAccessKeyLastUsed = Lens.lens (accessKeyLastUsed :: GetAccessKeyLastUsedResponse -> Lude.Maybe AccessKeyLastUsed) (\s a -> s {accessKeyLastUsed = a} :: GetAccessKeyLastUsedResponse)
{-# DEPRECATED gaklursAccessKeyLastUsed "Use generic-lens or generic-optics with 'accessKeyLastUsed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaklursResponseStatus :: Lens.Lens' GetAccessKeyLastUsedResponse Lude.Int
gaklursResponseStatus = Lens.lens (responseStatus :: GetAccessKeyLastUsedResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccessKeyLastUsedResponse)
{-# DEPRECATED gaklursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
