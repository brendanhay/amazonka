{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.APIStage where

import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.Lens
import Network.AWS.Prelude

-- | API stage name of the associated API stage in a usage plan.
--
--
--
-- /See:/ 'apiStage' smart constructor.
data APIStage = APIStage'
  { _asStage :: !(Maybe Text),
    _asApiId :: !(Maybe Text),
    _asThrottle :: !(Maybe (Map Text (ThrottleSettings)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APIStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asStage' - API stage name of the associated API stage in a usage plan.
--
-- * 'asApiId' - API Id of the associated API stage in a usage plan.
--
-- * 'asThrottle' - Map containing method level throttling information for API stage in a usage plan.
apiStage ::
  APIStage
apiStage =
  APIStage'
    { _asStage = Nothing,
      _asApiId = Nothing,
      _asThrottle = Nothing
    }

-- | API stage name of the associated API stage in a usage plan.
asStage :: Lens' APIStage (Maybe Text)
asStage = lens _asStage (\s a -> s {_asStage = a})

-- | API Id of the associated API stage in a usage plan.
asApiId :: Lens' APIStage (Maybe Text)
asApiId = lens _asApiId (\s a -> s {_asApiId = a})

-- | Map containing method level throttling information for API stage in a usage plan.
asThrottle :: Lens' APIStage (HashMap Text (ThrottleSettings))
asThrottle = lens _asThrottle (\s a -> s {_asThrottle = a}) . _Default . _Map

instance FromJSON APIStage where
  parseJSON =
    withObject
      "APIStage"
      ( \x ->
          APIStage'
            <$> (x .:? "stage")
            <*> (x .:? "apiId")
            <*> (x .:? "throttle" .!= mempty)
      )

instance Hashable APIStage

instance NFData APIStage

instance ToJSON APIStage where
  toJSON APIStage' {..} =
    object
      ( catMaybes
          [ ("stage" .=) <$> _asStage,
            ("apiId" .=) <$> _asApiId,
            ("throttle" .=) <$> _asThrottle
          ]
      )
