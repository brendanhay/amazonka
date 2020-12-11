{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of field-level encryption profiles that have been created in CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
  ( -- * Creating a request
    ListFieldLevelEncryptionProfiles (..),
    mkListFieldLevelEncryptionProfiles,

    -- ** Request lenses
    lflepMarker,
    lflepMaxItems,

    -- * Destructuring the response
    ListFieldLevelEncryptionProfilesResponse (..),
    mkListFieldLevelEncryptionProfilesResponse,

    -- ** Response lenses
    lfleprsFieldLevelEncryptionProfileList,
    lfleprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFieldLevelEncryptionProfiles' smart constructor.
data ListFieldLevelEncryptionProfiles = ListFieldLevelEncryptionProfiles'
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

-- | Creates a value of 'ListFieldLevelEncryptionProfiles' with the minimum fields required to make a request.
--
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
-- * 'maxItems' - The maximum number of field-level encryption profiles you want in the response body.
mkListFieldLevelEncryptionProfiles ::
  ListFieldLevelEncryptionProfiles
mkListFieldLevelEncryptionProfiles =
  ListFieldLevelEncryptionProfiles'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflepMarker :: Lens.Lens' ListFieldLevelEncryptionProfiles (Lude.Maybe Lude.Text)
lflepMarker = Lens.lens (marker :: ListFieldLevelEncryptionProfiles -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFieldLevelEncryptionProfiles)
{-# DEPRECATED lflepMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of field-level encryption profiles you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflepMaxItems :: Lens.Lens' ListFieldLevelEncryptionProfiles (Lude.Maybe Lude.Text)
lflepMaxItems = Lens.lens (maxItems :: ListFieldLevelEncryptionProfiles -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListFieldLevelEncryptionProfiles)
{-# DEPRECATED lflepMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListFieldLevelEncryptionProfiles where
  type
    Rs ListFieldLevelEncryptionProfiles =
      ListFieldLevelEncryptionProfilesResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionProfilesResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFieldLevelEncryptionProfiles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListFieldLevelEncryptionProfiles where
  toPath = Lude.const "/2020-05-31/field-level-encryption-profile"

instance Lude.ToQuery ListFieldLevelEncryptionProfiles where
  toQuery ListFieldLevelEncryptionProfiles' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListFieldLevelEncryptionProfilesResponse' smart constructor.
data ListFieldLevelEncryptionProfilesResponse = ListFieldLevelEncryptionProfilesResponse'
  { fieldLevelEncryptionProfileList ::
      Lude.Maybe
        FieldLevelEncryptionProfileList,
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

-- | Creates a value of 'ListFieldLevelEncryptionProfilesResponse' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionProfileList' - Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
-- * 'responseStatus' - The response status code.
mkListFieldLevelEncryptionProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFieldLevelEncryptionProfilesResponse
mkListFieldLevelEncryptionProfilesResponse pResponseStatus_ =
  ListFieldLevelEncryptionProfilesResponse'
    { fieldLevelEncryptionProfileList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfleprsFieldLevelEncryptionProfileList :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse (Lude.Maybe FieldLevelEncryptionProfileList)
lfleprsFieldLevelEncryptionProfileList = Lens.lens (fieldLevelEncryptionProfileList :: ListFieldLevelEncryptionProfilesResponse -> Lude.Maybe FieldLevelEncryptionProfileList) (\s a -> s {fieldLevelEncryptionProfileList = a} :: ListFieldLevelEncryptionProfilesResponse)
{-# DEPRECATED lfleprsFieldLevelEncryptionProfileList "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfleprsResponseStatus :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse Lude.Int
lfleprsResponseStatus = Lens.lens (responseStatus :: ListFieldLevelEncryptionProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFieldLevelEncryptionProfilesResponse)
{-# DEPRECATED lfleprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
