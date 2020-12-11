{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @SourceCredentialsInfo@ objects.
module Network.AWS.CodeBuild.ListSourceCredentials
  ( -- * Creating a request
    ListSourceCredentials (..),
    mkListSourceCredentials,

    -- * Destructuring the response
    ListSourceCredentialsResponse (..),
    mkListSourceCredentialsResponse,

    -- ** Response lenses
    lscrsSourceCredentialsInfos,
    lscrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSourceCredentials' smart constructor.
data ListSourceCredentials = ListSourceCredentials'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSourceCredentials' with the minimum fields required to make a request.
mkListSourceCredentials ::
  ListSourceCredentials
mkListSourceCredentials = ListSourceCredentials'

instance Lude.AWSRequest ListSourceCredentials where
  type Rs ListSourceCredentials = ListSourceCredentialsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSourceCredentialsResponse'
            Lude.<$> (x Lude..?> "sourceCredentialsInfos" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSourceCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListSourceCredentials" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSourceCredentials where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ListSourceCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSourceCredentials where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSourceCredentialsResponse' smart constructor.
data ListSourceCredentialsResponse = ListSourceCredentialsResponse'
  { sourceCredentialsInfos ::
      Lude.Maybe
        [SourceCredentialsInfo],
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

-- | Creates a value of 'ListSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sourceCredentialsInfos' - A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
mkListSourceCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSourceCredentialsResponse
mkListSourceCredentialsResponse pResponseStatus_ =
  ListSourceCredentialsResponse'
    { sourceCredentialsInfos =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
-- /Note:/ Consider using 'sourceCredentialsInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsSourceCredentialsInfos :: Lens.Lens' ListSourceCredentialsResponse (Lude.Maybe [SourceCredentialsInfo])
lscrsSourceCredentialsInfos = Lens.lens (sourceCredentialsInfos :: ListSourceCredentialsResponse -> Lude.Maybe [SourceCredentialsInfo]) (\s a -> s {sourceCredentialsInfos = a} :: ListSourceCredentialsResponse)
{-# DEPRECATED lscrsSourceCredentialsInfos "Use generic-lens or generic-optics with 'sourceCredentialsInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsResponseStatus :: Lens.Lens' ListSourceCredentialsResponse Lude.Int
lscrsResponseStatus = Lens.lens (responseStatus :: ListSourceCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSourceCredentialsResponse)
{-# DEPRECATED lscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
