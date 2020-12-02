{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsEntityItem

-- | The result of the query.
--
--
--
-- /See:/ 'opsEntity' smart constructor.
data OpsEntity = OpsEntity'
  { _oeData ::
      !(Maybe (Map Text (OpsEntityItem))),
    _oeId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeData' - The data returned by the query.
--
-- * 'oeId' - The query ID.
opsEntity ::
  OpsEntity
opsEntity = OpsEntity' {_oeData = Nothing, _oeId = Nothing}

-- | The data returned by the query.
oeData :: Lens' OpsEntity (HashMap Text (OpsEntityItem))
oeData = lens _oeData (\s a -> s {_oeData = a}) . _Default . _Map

-- | The query ID.
oeId :: Lens' OpsEntity (Maybe Text)
oeId = lens _oeId (\s a -> s {_oeId = a})

instance FromJSON OpsEntity where
  parseJSON =
    withObject
      "OpsEntity"
      (\x -> OpsEntity' <$> (x .:? "Data" .!= mempty) <*> (x .:? "Id"))

instance Hashable OpsEntity

instance NFData OpsEntity
