{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the directory.
--
--
--
-- /See:/ 'pathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { _ptoiObjectIdentifiers ::
      !(Maybe [Text]),
    _ptoiPath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PathToObjectIdentifiers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptoiObjectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
--
-- * 'ptoiPath' - The path that is used to identify the object starting from directory root.
pathToObjectIdentifiers ::
  PathToObjectIdentifiers
pathToObjectIdentifiers =
  PathToObjectIdentifiers'
    { _ptoiObjectIdentifiers = Nothing,
      _ptoiPath = Nothing
    }

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
ptoiObjectIdentifiers :: Lens' PathToObjectIdentifiers [Text]
ptoiObjectIdentifiers = lens _ptoiObjectIdentifiers (\s a -> s {_ptoiObjectIdentifiers = a}) . _Default . _Coerce

-- | The path that is used to identify the object starting from directory root.
ptoiPath :: Lens' PathToObjectIdentifiers (Maybe Text)
ptoiPath = lens _ptoiPath (\s a -> s {_ptoiPath = a})

instance FromJSON PathToObjectIdentifiers where
  parseJSON =
    withObject
      "PathToObjectIdentifiers"
      ( \x ->
          PathToObjectIdentifiers'
            <$> (x .:? "ObjectIdentifiers" .!= mempty) <*> (x .:? "Path")
      )

instance Hashable PathToObjectIdentifiers

instance NFData PathToObjectIdentifiers
