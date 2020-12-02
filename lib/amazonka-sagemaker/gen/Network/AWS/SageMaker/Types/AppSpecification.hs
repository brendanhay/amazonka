{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration to run a processing job in a specified container image.
--
--
--
-- /See:/ 'appSpecification' smart constructor.
data AppSpecification = AppSpecification'
  { _asContainerArguments ::
      !(Maybe (List1 Text)),
    _asContainerEntrypoint :: !(Maybe (List1 Text)),
    _asImageURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asContainerArguments' - The arguments for a container used to run a processing job.
--
-- * 'asContainerEntrypoint' - The entrypoint for a container used to run a processing job.
--
-- * 'asImageURI' - The container image to be run by the processing job.
appSpecification ::
  -- | 'asImageURI'
  Text ->
  AppSpecification
appSpecification pImageURI_ =
  AppSpecification'
    { _asContainerArguments = Nothing,
      _asContainerEntrypoint = Nothing,
      _asImageURI = pImageURI_
    }

-- | The arguments for a container used to run a processing job.
asContainerArguments :: Lens' AppSpecification (Maybe (NonEmpty Text))
asContainerArguments = lens _asContainerArguments (\s a -> s {_asContainerArguments = a}) . mapping _List1

-- | The entrypoint for a container used to run a processing job.
asContainerEntrypoint :: Lens' AppSpecification (Maybe (NonEmpty Text))
asContainerEntrypoint = lens _asContainerEntrypoint (\s a -> s {_asContainerEntrypoint = a}) . mapping _List1

-- | The container image to be run by the processing job.
asImageURI :: Lens' AppSpecification Text
asImageURI = lens _asImageURI (\s a -> s {_asImageURI = a})

instance FromJSON AppSpecification where
  parseJSON =
    withObject
      "AppSpecification"
      ( \x ->
          AppSpecification'
            <$> (x .:? "ContainerArguments")
            <*> (x .:? "ContainerEntrypoint")
            <*> (x .: "ImageUri")
      )

instance Hashable AppSpecification

instance NFData AppSpecification

instance ToJSON AppSpecification where
  toJSON AppSpecification' {..} =
    object
      ( catMaybes
          [ ("ContainerArguments" .=) <$> _asContainerArguments,
            ("ContainerEntrypoint" .=) <$> _asContainerEntrypoint,
            Just ("ImageUri" .= _asImageURI)
          ]
      )
