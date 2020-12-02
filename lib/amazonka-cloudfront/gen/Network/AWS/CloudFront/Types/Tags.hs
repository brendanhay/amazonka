{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Tags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tags where

import Network.AWS.CloudFront.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains zero or more @Tag@ elements.
--
--
--
-- /See:/ 'tags' smart constructor.
newtype Tags = Tags' {_tItems :: Maybe [Tag]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tItems' - A complex type that contains @Tag@ elements.
tags ::
  Tags
tags = Tags' {_tItems = Nothing}

-- | A complex type that contains @Tag@ elements.
tItems :: Lens' Tags [Tag]
tItems = lens _tItems (\s a -> s {_tItems = a}) . _Default . _Coerce

instance FromXML Tags where
  parseXML x =
    Tags' <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable Tags

instance NFData Tags

instance ToXML Tags where
  toXML Tags' {..} =
    mconcat ["Items" @= toXML (toXMLList "Tag" <$> _tItems)]
