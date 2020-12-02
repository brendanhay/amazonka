{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3KeyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.S3KeyFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FilterRule

-- | A container for object key name prefix and suffix filtering rules.
--
--
--
-- /See:/ 's3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
  { _skfFilterRules ::
      Maybe [FilterRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3KeyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skfFilterRules' - Undocumented member.
s3KeyFilter ::
  S3KeyFilter
s3KeyFilter = S3KeyFilter' {_skfFilterRules = Nothing}

-- | Undocumented member.
skfFilterRules :: Lens' S3KeyFilter [FilterRule]
skfFilterRules = lens _skfFilterRules (\s a -> s {_skfFilterRules = a}) . _Default . _Coerce

instance FromXML S3KeyFilter where
  parseXML x = S3KeyFilter' <$> (may (parseXMLList "FilterRule") x)

instance Hashable S3KeyFilter

instance NFData S3KeyFilter

instance ToXML S3KeyFilter where
  toXML S3KeyFilter' {..} =
    mconcat [toXML (toXMLList "FilterRule" <$> _skfFilterRules)]
