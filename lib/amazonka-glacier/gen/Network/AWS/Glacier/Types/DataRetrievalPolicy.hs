{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalPolicy where

import Network.AWS.Glacier.Types.DataRetrievalRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Data retrieval policy.
--
--
--
-- /See:/ 'dataRetrievalPolicy' smart constructor.
newtype DataRetrievalPolicy = DataRetrievalPolicy'
  { _drpRules ::
      Maybe [DataRetrievalRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataRetrievalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRules' - The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
dataRetrievalPolicy ::
  DataRetrievalPolicy
dataRetrievalPolicy = DataRetrievalPolicy' {_drpRules = Nothing}

-- | The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
drpRules :: Lens' DataRetrievalPolicy [DataRetrievalRule]
drpRules = lens _drpRules (\s a -> s {_drpRules = a}) . _Default . _Coerce

instance FromJSON DataRetrievalPolicy where
  parseJSON =
    withObject
      "DataRetrievalPolicy"
      (\x -> DataRetrievalPolicy' <$> (x .:? "Rules" .!= mempty))

instance Hashable DataRetrievalPolicy

instance NFData DataRetrievalPolicy

instance ToJSON DataRetrievalPolicy where
  toJSON DataRetrievalPolicy' {..} =
    object (catMaybes [("Rules" .=) <$> _drpRules])
