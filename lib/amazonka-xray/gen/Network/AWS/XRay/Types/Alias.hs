{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Alias where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An alias for an edge.
--
--
--
-- /See:/ 'alias' smart constructor.
data Alias = Alias'
  { _aNames :: !(Maybe [Text]),
    _aName :: !(Maybe Text),
    _aType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aNames' - A list of names for the alias, including the canonical name.
--
-- * 'aName' - The canonical name of the alias.
--
-- * 'aType' - The type of the alias.
alias ::
  Alias
alias =
  Alias' {_aNames = Nothing, _aName = Nothing, _aType = Nothing}

-- | A list of names for the alias, including the canonical name.
aNames :: Lens' Alias [Text]
aNames = lens _aNames (\s a -> s {_aNames = a}) . _Default . _Coerce

-- | The canonical name of the alias.
aName :: Lens' Alias (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | The type of the alias.
aType :: Lens' Alias (Maybe Text)
aType = lens _aType (\s a -> s {_aType = a})

instance FromJSON Alias where
  parseJSON =
    withObject
      "Alias"
      ( \x ->
          Alias'
            <$> (x .:? "Names" .!= mempty) <*> (x .:? "Name") <*> (x .:? "Type")
      )

instance Hashable Alias

instance NFData Alias
