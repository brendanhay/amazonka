{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IdFormat where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the ID format for a resource.
--
--
--
-- /See:/ 'idFormat' smart constructor.
data IdFormat = IdFormat'
  { _ifUseLongIds :: !(Maybe Bool),
    _ifDeadline :: !(Maybe ISO8601),
    _ifResource :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifUseLongIds' - Indicates whether longer IDs (17-character IDs) are enabled for the resource.
--
-- * 'ifDeadline' - The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
--
-- * 'ifResource' - The type of resource.
idFormat ::
  IdFormat
idFormat =
  IdFormat'
    { _ifUseLongIds = Nothing,
      _ifDeadline = Nothing,
      _ifResource = Nothing
    }

-- | Indicates whether longer IDs (17-character IDs) are enabled for the resource.
ifUseLongIds :: Lens' IdFormat (Maybe Bool)
ifUseLongIds = lens _ifUseLongIds (\s a -> s {_ifUseLongIds = a})

-- | The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
ifDeadline :: Lens' IdFormat (Maybe UTCTime)
ifDeadline = lens _ifDeadline (\s a -> s {_ifDeadline = a}) . mapping _Time

-- | The type of resource.
ifResource :: Lens' IdFormat (Maybe Text)
ifResource = lens _ifResource (\s a -> s {_ifResource = a})

instance FromXML IdFormat where
  parseXML x =
    IdFormat'
      <$> (x .@? "useLongIds") <*> (x .@? "deadline") <*> (x .@? "resource")

instance Hashable IdFormat

instance NFData IdFormat
