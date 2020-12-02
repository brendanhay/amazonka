{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfile where

import Network.AWS.CloudFront.Types.Format
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A field-level encryption content type profile.
--
--
--
-- /See:/ 'contentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { _ctpProfileId ::
      !(Maybe Text),
    _ctpFormat :: !Format,
    _ctpContentType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContentTypeProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpProfileId' - The profile ID for a field-level encryption content type-profile mapping.
--
-- * 'ctpFormat' - The format for a field-level encryption content type-profile mapping.
--
-- * 'ctpContentType' - The content type for a field-level encryption content type-profile mapping.
contentTypeProfile ::
  -- | 'ctpFormat'
  Format ->
  -- | 'ctpContentType'
  Text ->
  ContentTypeProfile
contentTypeProfile pFormat_ pContentType_ =
  ContentTypeProfile'
    { _ctpProfileId = Nothing,
      _ctpFormat = pFormat_,
      _ctpContentType = pContentType_
    }

-- | The profile ID for a field-level encryption content type-profile mapping.
ctpProfileId :: Lens' ContentTypeProfile (Maybe Text)
ctpProfileId = lens _ctpProfileId (\s a -> s {_ctpProfileId = a})

-- | The format for a field-level encryption content type-profile mapping.
ctpFormat :: Lens' ContentTypeProfile Format
ctpFormat = lens _ctpFormat (\s a -> s {_ctpFormat = a})

-- | The content type for a field-level encryption content type-profile mapping.
ctpContentType :: Lens' ContentTypeProfile Text
ctpContentType = lens _ctpContentType (\s a -> s {_ctpContentType = a})

instance FromXML ContentTypeProfile where
  parseXML x =
    ContentTypeProfile'
      <$> (x .@? "ProfileId") <*> (x .@ "Format") <*> (x .@ "ContentType")

instance Hashable ContentTypeProfile

instance NFData ContentTypeProfile

instance ToXML ContentTypeProfile where
  toXML ContentTypeProfile' {..} =
    mconcat
      [ "ProfileId" @= _ctpProfileId,
        "Format" @= _ctpFormat,
        "ContentType" @= _ctpContentType
      ]
