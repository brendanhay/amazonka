{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary

-- | An object containing detailed information about the self-service action.
--
--
--
-- /See:/ 'serviceActionDetail' smart constructor.
data ServiceActionDetail = ServiceActionDetail'
  { _sadServiceActionSummary ::
      !(Maybe ServiceActionSummary),
    _sadDefinition ::
      !(Maybe (Map ServiceActionDefinitionKey (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceActionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sadServiceActionSummary' - Summary information about the self-service action.
--
-- * 'sadDefinition' - A map that defines the self-service action.
serviceActionDetail ::
  ServiceActionDetail
serviceActionDetail =
  ServiceActionDetail'
    { _sadServiceActionSummary = Nothing,
      _sadDefinition = Nothing
    }

-- | Summary information about the self-service action.
sadServiceActionSummary :: Lens' ServiceActionDetail (Maybe ServiceActionSummary)
sadServiceActionSummary = lens _sadServiceActionSummary (\s a -> s {_sadServiceActionSummary = a})

-- | A map that defines the self-service action.
sadDefinition :: Lens' ServiceActionDetail (HashMap ServiceActionDefinitionKey (Text))
sadDefinition = lens _sadDefinition (\s a -> s {_sadDefinition = a}) . _Default . _Map

instance FromJSON ServiceActionDetail where
  parseJSON =
    withObject
      "ServiceActionDetail"
      ( \x ->
          ServiceActionDetail'
            <$> (x .:? "ServiceActionSummary") <*> (x .:? "Definition" .!= mempty)
      )

instance Hashable ServiceActionDetail

instance NFData ServiceActionDetail
