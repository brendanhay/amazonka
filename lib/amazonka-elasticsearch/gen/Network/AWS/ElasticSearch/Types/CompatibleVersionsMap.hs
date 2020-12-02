{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CompatibleVersionsMap where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A map from an @'ElasticsearchVersion' @ to a list of compatible @'ElasticsearchVersion' @ s to which the domain can be upgraded.
--
--
--
-- /See:/ 'compatibleVersionsMap' smart constructor.
data CompatibleVersionsMap = CompatibleVersionsMap'
  { _cvmSourceVersion ::
      !(Maybe Text),
    _cvmTargetVersions :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompatibleVersionsMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvmSourceVersion' - The current version of Elasticsearch on which a domain is.
--
-- * 'cvmTargetVersions' - Undocumented member.
compatibleVersionsMap ::
  CompatibleVersionsMap
compatibleVersionsMap =
  CompatibleVersionsMap'
    { _cvmSourceVersion = Nothing,
      _cvmTargetVersions = Nothing
    }

-- | The current version of Elasticsearch on which a domain is.
cvmSourceVersion :: Lens' CompatibleVersionsMap (Maybe Text)
cvmSourceVersion = lens _cvmSourceVersion (\s a -> s {_cvmSourceVersion = a})

-- | Undocumented member.
cvmTargetVersions :: Lens' CompatibleVersionsMap [Text]
cvmTargetVersions = lens _cvmTargetVersions (\s a -> s {_cvmTargetVersions = a}) . _Default . _Coerce

instance FromJSON CompatibleVersionsMap where
  parseJSON =
    withObject
      "CompatibleVersionsMap"
      ( \x ->
          CompatibleVersionsMap'
            <$> (x .:? "SourceVersion") <*> (x .:? "TargetVersions" .!= mempty)
      )

instance Hashable CompatibleVersionsMap

instance NFData CompatibleVersionsMap
