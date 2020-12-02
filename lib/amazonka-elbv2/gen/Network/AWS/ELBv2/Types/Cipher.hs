{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Cipher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Cipher where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a cipher used in a policy.
--
--
--
-- /See:/ 'cipher' smart constructor.
data Cipher = Cipher'
  { _cPriority :: !(Maybe Int),
    _cName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Cipher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPriority' - The priority of the cipher.
--
-- * 'cName' - The name of the cipher.
cipher ::
  Cipher
cipher = Cipher' {_cPriority = Nothing, _cName = Nothing}

-- | The priority of the cipher.
cPriority :: Lens' Cipher (Maybe Int)
cPriority = lens _cPriority (\s a -> s {_cPriority = a})

-- | The name of the cipher.
cName :: Lens' Cipher (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

instance FromXML Cipher where
  parseXML x = Cipher' <$> (x .@? "Priority") <*> (x .@? "Name")

instance Hashable Cipher

instance NFData Cipher
