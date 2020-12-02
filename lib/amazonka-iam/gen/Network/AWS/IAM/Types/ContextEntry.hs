{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextEntry where

import Network.AWS.IAM.Types.ContextKeyTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the @Condition@ elements of the input policies.
--
--
-- This data type is used as an input parameter to 'SimulateCustomPolicy' and 'SimulatePrincipalPolicy' .
--
--
-- /See:/ 'contextEntry' smart constructor.
data ContextEntry = ContextEntry'
  { _ceContextKeyValues ::
      !(Maybe [Text]),
    _ceContextKeyName :: !(Maybe Text),
    _ceContextKeyType :: !(Maybe ContextKeyTypeEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContextEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceContextKeyValues' - The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
--
-- * 'ceContextKeyName' - The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
--
-- * 'ceContextKeyType' - The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
contextEntry ::
  ContextEntry
contextEntry =
  ContextEntry'
    { _ceContextKeyValues = Nothing,
      _ceContextKeyName = Nothing,
      _ceContextKeyType = Nothing
    }

-- | The value (or values, if the condition context key supports multiple values) to provide to the simulation when the key is referenced by a @Condition@ element in an input policy.
ceContextKeyValues :: Lens' ContextEntry [Text]
ceContextKeyValues = lens _ceContextKeyValues (\s a -> s {_ceContextKeyValues = a}) . _Default . _Coerce

-- | The full name of a condition context key, including the service prefix. For example, @aws:SourceIp@ or @s3:VersionId@ .
ceContextKeyName :: Lens' ContextEntry (Maybe Text)
ceContextKeyName = lens _ceContextKeyName (\s a -> s {_ceContextKeyName = a})

-- | The data type of the value (or values) specified in the @ContextKeyValues@ parameter.
ceContextKeyType :: Lens' ContextEntry (Maybe ContextKeyTypeEnum)
ceContextKeyType = lens _ceContextKeyType (\s a -> s {_ceContextKeyType = a})

instance Hashable ContextEntry

instance NFData ContextEntry

instance ToQuery ContextEntry where
  toQuery ContextEntry' {..} =
    mconcat
      [ "ContextKeyValues"
          =: toQuery (toQueryList "member" <$> _ceContextKeyValues),
        "ContextKeyName" =: _ceContextKeyName,
        "ContextKeyType" =: _ceContextKeyType
      ]
