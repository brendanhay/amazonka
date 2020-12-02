{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the details of an artifact.
--
--
--
-- /See:/ 'artifactDetails' smart constructor.
data ArtifactDetails = ArtifactDetails'
  { _adMinimumCount :: !Nat,
    _adMaximumCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArtifactDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adMinimumCount' - The minimum number of artifacts allowed for the action type.
--
-- * 'adMaximumCount' - The maximum number of artifacts allowed for the action type.
artifactDetails ::
  -- | 'adMinimumCount'
  Natural ->
  -- | 'adMaximumCount'
  Natural ->
  ArtifactDetails
artifactDetails pMinimumCount_ pMaximumCount_ =
  ArtifactDetails'
    { _adMinimumCount = _Nat # pMinimumCount_,
      _adMaximumCount = _Nat # pMaximumCount_
    }

-- | The minimum number of artifacts allowed for the action type.
adMinimumCount :: Lens' ArtifactDetails Natural
adMinimumCount = lens _adMinimumCount (\s a -> s {_adMinimumCount = a}) . _Nat

-- | The maximum number of artifacts allowed for the action type.
adMaximumCount :: Lens' ArtifactDetails Natural
adMaximumCount = lens _adMaximumCount (\s a -> s {_adMaximumCount = a}) . _Nat

instance FromJSON ArtifactDetails where
  parseJSON =
    withObject
      "ArtifactDetails"
      ( \x ->
          ArtifactDetails'
            <$> (x .: "minimumCount") <*> (x .: "maximumCount")
      )

instance Hashable ArtifactDetails

instance NFData ArtifactDetails

instance ToJSON ArtifactDetails where
  toJSON ArtifactDetails' {..} =
    object
      ( catMaybes
          [ Just ("minimumCount" .= _adMinimumCount),
            Just ("maximumCount" .= _adMaximumCount)
          ]
      )
