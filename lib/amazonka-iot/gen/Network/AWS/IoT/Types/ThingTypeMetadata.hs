{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.
--
--
--
-- /See:/ 'thingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { _ttmDeprecationDate ::
      !(Maybe POSIX),
    _ttmCreationDate :: !(Maybe POSIX),
    _ttmDeprecated :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingTypeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttmDeprecationDate' - The date and time when the thing type was deprecated.
--
-- * 'ttmCreationDate' - The date and time when the thing type was created.
--
-- * 'ttmDeprecated' - Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
thingTypeMetadata ::
  ThingTypeMetadata
thingTypeMetadata =
  ThingTypeMetadata'
    { _ttmDeprecationDate = Nothing,
      _ttmCreationDate = Nothing,
      _ttmDeprecated = Nothing
    }

-- | The date and time when the thing type was deprecated.
ttmDeprecationDate :: Lens' ThingTypeMetadata (Maybe UTCTime)
ttmDeprecationDate = lens _ttmDeprecationDate (\s a -> s {_ttmDeprecationDate = a}) . mapping _Time

-- | The date and time when the thing type was created.
ttmCreationDate :: Lens' ThingTypeMetadata (Maybe UTCTime)
ttmCreationDate = lens _ttmCreationDate (\s a -> s {_ttmCreationDate = a}) . mapping _Time

-- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
ttmDeprecated :: Lens' ThingTypeMetadata (Maybe Bool)
ttmDeprecated = lens _ttmDeprecated (\s a -> s {_ttmDeprecated = a})

instance FromJSON ThingTypeMetadata where
  parseJSON =
    withObject
      "ThingTypeMetadata"
      ( \x ->
          ThingTypeMetadata'
            <$> (x .:? "deprecationDate")
            <*> (x .:? "creationDate")
            <*> (x .:? "deprecated")
      )

instance Hashable ThingTypeMetadata

instance NFData ThingTypeMetadata
