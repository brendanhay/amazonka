{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Query where

import Network.AWS.DataPipeline.Types.Selector
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the query to run against an object.
--
--
--
-- /See:/ 'query' smart constructor.
newtype Query = Query' {_qSelectors :: Maybe [Selector]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Query' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qSelectors' - List of selectors that define the query. An object must satisfy all of the selectors to match the query.
query ::
  Query
query = Query' {_qSelectors = Nothing}

-- | List of selectors that define the query. An object must satisfy all of the selectors to match the query.
qSelectors :: Lens' Query [Selector]
qSelectors = lens _qSelectors (\s a -> s {_qSelectors = a}) . _Default . _Coerce

instance Hashable Query

instance NFData Query

instance ToJSON Query where
  toJSON Query' {..} =
    object (catMaybes [("selectors" .=) <$> _qSelectors])
