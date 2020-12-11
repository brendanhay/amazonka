{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all field-level encryption configurations that have been created in CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
  ( -- * Creating a request
    ListFieldLevelEncryptionConfigs (..),
    mkListFieldLevelEncryptionConfigs,

    -- ** Request lenses
    lflecMarker,
    lflecMaxItems,

    -- * Destructuring the response
    ListFieldLevelEncryptionConfigsResponse (..),
    mkListFieldLevelEncryptionConfigsResponse,

    -- ** Response lenses
    lflecrsFieldLevelEncryptionList,
    lflecrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems ::
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

-- | Creates a value of 'ListFieldLevelEncryptionConfigs' with the minimum fields required to make a request.
--
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
-- * 'maxItems' - The maximum number of field-level encryption configurations you want in the response body.
mkListFieldLevelEncryptionConfigs ::
  ListFieldLevelEncryptionConfigs
mkListFieldLevelEncryptionConfigs =
  ListFieldLevelEncryptionConfigs'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMarker :: Lens.Lens' ListFieldLevelEncryptionConfigs (Lude.Maybe Lude.Text)
lflecMarker = Lens.lens (marker :: ListFieldLevelEncryptionConfigs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFieldLevelEncryptionConfigs)
{-# DEPRECATED lflecMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of field-level encryption configurations you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMaxItems :: Lens.Lens' ListFieldLevelEncryptionConfigs (Lude.Maybe Lude.Text)
lflecMaxItems = Lens.lens (maxItems :: ListFieldLevelEncryptionConfigs -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListFieldLevelEncryptionConfigs)
{-# DEPRECATED lflecMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListFieldLevelEncryptionConfigs where
  type
    Rs ListFieldLevelEncryptionConfigs =
      ListFieldLevelEncryptionConfigsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionConfigsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFieldLevelEncryptionConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListFieldLevelEncryptionConfigs where
  toPath = Lude.const "/2020-05-31/field-level-encryption"

instance Lude.ToQuery ListFieldLevelEncryptionConfigs where
  toQuery ListFieldLevelEncryptionConfigs' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListFieldLevelEncryptionConfigsResponse' smart constructor.
data ListFieldLevelEncryptionConfigsResponse = ListFieldLevelEncryptionConfigsResponse'
  { fieldLevelEncryptionList ::
      Lude.Maybe
        FieldLevelEncryptionList,
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

-- | Creates a value of 'ListFieldLevelEncryptionConfigsResponse' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionList' - Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
-- * 'responseStatus' - The response status code.
mkListFieldLevelEncryptionConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFieldLevelEncryptionConfigsResponse
mkListFieldLevelEncryptionConfigsResponse pResponseStatus_ =
  ListFieldLevelEncryptionConfigsResponse'
    { fieldLevelEncryptionList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
--
-- /Note:/ Consider using 'fieldLevelEncryptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrsFieldLevelEncryptionList :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse (Lude.Maybe FieldLevelEncryptionList)
lflecrsFieldLevelEncryptionList = Lens.lens (fieldLevelEncryptionList :: ListFieldLevelEncryptionConfigsResponse -> Lude.Maybe FieldLevelEncryptionList) (\s a -> s {fieldLevelEncryptionList = a} :: ListFieldLevelEncryptionConfigsResponse)
{-# DEPRECATED lflecrsFieldLevelEncryptionList "Use generic-lens or generic-optics with 'fieldLevelEncryptionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrsResponseStatus :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse Lude.Int
lflecrsResponseStatus = Lens.lens (responseStatus :: ListFieldLevelEncryptionConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFieldLevelEncryptionConfigsResponse)
{-# DEPRECATED lflecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
