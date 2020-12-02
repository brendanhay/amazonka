{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.CreatedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.CreatedArtifact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).
--
--
--
-- /See:/ 'createdArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { _caDescription ::
      !(Maybe Text),
    _caName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatedArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDescription' - A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
--
-- * 'caName' - An ARN that uniquely identifies the result of a migration task.
createdArtifact ::
  -- | 'caName'
  Text ->
  CreatedArtifact
createdArtifact pName_ =
  CreatedArtifact' {_caDescription = Nothing, _caName = pName_}

-- | A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
caDescription :: Lens' CreatedArtifact (Maybe Text)
caDescription = lens _caDescription (\s a -> s {_caDescription = a})

-- | An ARN that uniquely identifies the result of a migration task.
caName :: Lens' CreatedArtifact Text
caName = lens _caName (\s a -> s {_caName = a})

instance FromJSON CreatedArtifact where
  parseJSON =
    withObject
      "CreatedArtifact"
      ( \x ->
          CreatedArtifact' <$> (x .:? "Description") <*> (x .: "Name")
      )

instance Hashable CreatedArtifact

instance NFData CreatedArtifact

instance ToJSON CreatedArtifact where
  toJSON CreatedArtifact' {..} =
    object
      ( catMaybes
          [("Description" .=) <$> _caDescription, Just ("Name" .= _caName)]
      )
