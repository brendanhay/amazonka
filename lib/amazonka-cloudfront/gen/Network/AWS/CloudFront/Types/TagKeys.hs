{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TagKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TagKeys where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains zero or more @Tag@ elements.
--
--
--
-- /See:/ 'tagKeys' smart constructor.
newtype TagKeys = TagKeys' {_tkItems :: Maybe [Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tkItems' - A complex type that contains @Tag@ key elements.
tagKeys ::
  TagKeys
tagKeys = TagKeys' {_tkItems = Nothing}

-- | A complex type that contains @Tag@ key elements.
tkItems :: Lens' TagKeys [Text]
tkItems = lens _tkItems (\s a -> s {_tkItems = a}) . _Default . _Coerce

instance Hashable TagKeys

instance NFData TagKeys

instance ToXML TagKeys where
  toXML TagKeys' {..} =
    mconcat ["Items" @= toXML (toXMLList "Key" <$> _tkItems)]
