{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfileConfig where

import Network.AWS.CloudFront.Types.ContentTypeProfiles
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration for a field-level encryption content type-profile mapping.
--
--
--
-- /See:/ 'contentTypeProfileConfig' smart constructor.
data ContentTypeProfileConfig = ContentTypeProfileConfig'
  { _ctpcContentTypeProfiles ::
      !(Maybe ContentTypeProfiles),
    _ctpcForwardWhenContentTypeIsUnknown ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContentTypeProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpcContentTypeProfiles' - The configuration for a field-level encryption content type-profile.
--
-- * 'ctpcForwardWhenContentTypeIsUnknown' - The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
contentTypeProfileConfig ::
  -- | 'ctpcForwardWhenContentTypeIsUnknown'
  Bool ->
  ContentTypeProfileConfig
contentTypeProfileConfig pForwardWhenContentTypeIsUnknown_ =
  ContentTypeProfileConfig'
    { _ctpcContentTypeProfiles = Nothing,
      _ctpcForwardWhenContentTypeIsUnknown =
        pForwardWhenContentTypeIsUnknown_
    }

-- | The configuration for a field-level encryption content type-profile.
ctpcContentTypeProfiles :: Lens' ContentTypeProfileConfig (Maybe ContentTypeProfiles)
ctpcContentTypeProfiles = lens _ctpcContentTypeProfiles (\s a -> s {_ctpcContentTypeProfiles = a})

-- | The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
ctpcForwardWhenContentTypeIsUnknown :: Lens' ContentTypeProfileConfig Bool
ctpcForwardWhenContentTypeIsUnknown = lens _ctpcForwardWhenContentTypeIsUnknown (\s a -> s {_ctpcForwardWhenContentTypeIsUnknown = a})

instance FromXML ContentTypeProfileConfig where
  parseXML x =
    ContentTypeProfileConfig'
      <$> (x .@? "ContentTypeProfiles")
      <*> (x .@ "ForwardWhenContentTypeIsUnknown")

instance Hashable ContentTypeProfileConfig

instance NFData ContentTypeProfileConfig

instance ToXML ContentTypeProfileConfig where
  toXML ContentTypeProfileConfig' {..} =
    mconcat
      [ "ContentTypeProfiles" @= _ctpcContentTypeProfiles,
        "ForwardWhenContentTypeIsUnknown"
          @= _ctpcForwardWhenContentTypeIsUnknown
      ]
