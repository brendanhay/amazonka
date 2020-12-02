{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionResultsMessage where

import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'updateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { _uarmUnprocessedUpdateActions ::
      !(Maybe [UnprocessedUpdateAction]),
    _uarmProcessedUpdateActions ::
      !(Maybe [ProcessedUpdateAction])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateActionResultsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarmUnprocessedUpdateActions' - Update actions that haven't been processed successfully
--
-- * 'uarmProcessedUpdateActions' - Update actions that have been processed successfully
updateActionResultsMessage ::
  UpdateActionResultsMessage
updateActionResultsMessage =
  UpdateActionResultsMessage'
    { _uarmUnprocessedUpdateActions =
        Nothing,
      _uarmProcessedUpdateActions = Nothing
    }

-- | Update actions that haven't been processed successfully
uarmUnprocessedUpdateActions :: Lens' UpdateActionResultsMessage [UnprocessedUpdateAction]
uarmUnprocessedUpdateActions = lens _uarmUnprocessedUpdateActions (\s a -> s {_uarmUnprocessedUpdateActions = a}) . _Default . _Coerce

-- | Update actions that have been processed successfully
uarmProcessedUpdateActions :: Lens' UpdateActionResultsMessage [ProcessedUpdateAction]
uarmProcessedUpdateActions = lens _uarmProcessedUpdateActions (\s a -> s {_uarmProcessedUpdateActions = a}) . _Default . _Coerce

instance FromXML UpdateActionResultsMessage where
  parseXML x =
    UpdateActionResultsMessage'
      <$> ( x .@? "UnprocessedUpdateActions" .!@ mempty
              >>= may (parseXMLList "UnprocessedUpdateAction")
          )
      <*> ( x .@? "ProcessedUpdateActions" .!@ mempty
              >>= may (parseXMLList "ProcessedUpdateAction")
          )

instance Hashable UpdateActionResultsMessage

instance NFData UpdateActionResultsMessage
