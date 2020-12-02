{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.QueryInfo where

import Network.AWS.Config.Types.FieldInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the query.
--
--
--
-- /See:/ 'queryInfo' smart constructor.
newtype QueryInfo = QueryInfo'
  { _qiSelectFields ::
      Maybe [FieldInfo]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qiSelectFields' - Returns a @FieldInfo@ object.
queryInfo ::
  QueryInfo
queryInfo = QueryInfo' {_qiSelectFields = Nothing}

-- | Returns a @FieldInfo@ object.
qiSelectFields :: Lens' QueryInfo [FieldInfo]
qiSelectFields = lens _qiSelectFields (\s a -> s {_qiSelectFields = a}) . _Default . _Coerce

instance FromJSON QueryInfo where
  parseJSON =
    withObject
      "QueryInfo"
      (\x -> QueryInfo' <$> (x .:? "SelectFields" .!= mempty))

instance Hashable QueryInfo

instance NFData QueryInfo
