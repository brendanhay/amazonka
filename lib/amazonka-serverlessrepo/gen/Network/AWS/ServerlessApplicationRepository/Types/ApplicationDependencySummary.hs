{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A nested application summary.
--
--
--
-- /See:/ 'applicationDependencySummary' smart constructor.
data ApplicationDependencySummary = ApplicationDependencySummary'
  { _adsApplicationId ::
      !Text,
    _adsSemanticVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationDependencySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adsApplicationId' - The Amazon Resource Name (ARN) of the nested application.
--
-- * 'adsSemanticVersion' - The semantic version of the nested application.
applicationDependencySummary ::
  -- | 'adsApplicationId'
  Text ->
  -- | 'adsSemanticVersion'
  Text ->
  ApplicationDependencySummary
applicationDependencySummary pApplicationId_ pSemanticVersion_ =
  ApplicationDependencySummary'
    { _adsApplicationId =
        pApplicationId_,
      _adsSemanticVersion = pSemanticVersion_
    }

-- | The Amazon Resource Name (ARN) of the nested application.
adsApplicationId :: Lens' ApplicationDependencySummary Text
adsApplicationId = lens _adsApplicationId (\s a -> s {_adsApplicationId = a})

-- | The semantic version of the nested application.
adsSemanticVersion :: Lens' ApplicationDependencySummary Text
adsSemanticVersion = lens _adsSemanticVersion (\s a -> s {_adsSemanticVersion = a})

instance FromJSON ApplicationDependencySummary where
  parseJSON =
    withObject
      "ApplicationDependencySummary"
      ( \x ->
          ApplicationDependencySummary'
            <$> (x .: "applicationId") <*> (x .: "semanticVersion")
      )

instance Hashable ApplicationDependencySummary

instance NFData ApplicationDependencySummary
