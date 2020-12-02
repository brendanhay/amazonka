{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLIdp where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the SAML Identity Provider's information.
--
--
--
-- /See:/ 'sAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { _samliMetadataContent :: !Text,
    _samliEntityId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SAMLIdp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samliMetadataContent' - The Metadata of the SAML application in xml format.
--
-- * 'samliEntityId' - The unique Entity ID of the application in SAML Identity Provider.
sAMLIdp ::
  -- | 'samliMetadataContent'
  Text ->
  -- | 'samliEntityId'
  Text ->
  SAMLIdp
sAMLIdp pMetadataContent_ pEntityId_ =
  SAMLIdp'
    { _samliMetadataContent = pMetadataContent_,
      _samliEntityId = pEntityId_
    }

-- | The Metadata of the SAML application in xml format.
samliMetadataContent :: Lens' SAMLIdp Text
samliMetadataContent = lens _samliMetadataContent (\s a -> s {_samliMetadataContent = a})

-- | The unique Entity ID of the application in SAML Identity Provider.
samliEntityId :: Lens' SAMLIdp Text
samliEntityId = lens _samliEntityId (\s a -> s {_samliEntityId = a})

instance FromJSON SAMLIdp where
  parseJSON =
    withObject
      "SAMLIdp"
      ( \x ->
          SAMLIdp' <$> (x .: "MetadataContent") <*> (x .: "EntityId")
      )

instance Hashable SAMLIdp

instance NFData SAMLIdp

instance ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    object
      ( catMaybes
          [ Just ("MetadataContent" .= _samliMetadataContent),
            Just ("EntityId" .= _samliEntityId)
          ]
      )
