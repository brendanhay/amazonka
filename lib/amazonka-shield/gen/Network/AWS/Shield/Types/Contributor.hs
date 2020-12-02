{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Contributor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Contributor where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A contributor to the attack and their contribution.
--
--
--
-- /See:/ 'contributor' smart constructor.
data Contributor = Contributor'
  { _cValue :: !(Maybe Integer),
    _cName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Contributor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cValue' - The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
--
-- * 'cName' - The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
contributor ::
  Contributor
contributor = Contributor' {_cValue = Nothing, _cName = Nothing}

-- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
cValue :: Lens' Contributor (Maybe Integer)
cValue = lens _cValue (\s a -> s {_cValue = a})

-- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
cName :: Lens' Contributor (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

instance FromJSON Contributor where
  parseJSON =
    withObject
      "Contributor"
      (\x -> Contributor' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable Contributor

instance NFData Contributor
