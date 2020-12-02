{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfileConfig where

import Network.AWS.CloudFront.Types.QueryArgProfiles
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { _qapcQueryArgProfiles ::
      !(Maybe QueryArgProfiles),
    _qapcForwardWhenQueryArgProfileIsUnknown ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryArgProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapcQueryArgProfiles' - Profiles specified for query argument-profile mapping for field-level encryption.
--
-- * 'qapcForwardWhenQueryArgProfileIsUnknown' - Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
queryArgProfileConfig ::
  -- | 'qapcForwardWhenQueryArgProfileIsUnknown'
  Bool ->
  QueryArgProfileConfig
queryArgProfileConfig pForwardWhenQueryArgProfileIsUnknown_ =
  QueryArgProfileConfig'
    { _qapcQueryArgProfiles = Nothing,
      _qapcForwardWhenQueryArgProfileIsUnknown =
        pForwardWhenQueryArgProfileIsUnknown_
    }

-- | Profiles specified for query argument-profile mapping for field-level encryption.
qapcQueryArgProfiles :: Lens' QueryArgProfileConfig (Maybe QueryArgProfiles)
qapcQueryArgProfiles = lens _qapcQueryArgProfiles (\s a -> s {_qapcQueryArgProfiles = a})

-- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
qapcForwardWhenQueryArgProfileIsUnknown :: Lens' QueryArgProfileConfig Bool
qapcForwardWhenQueryArgProfileIsUnknown = lens _qapcForwardWhenQueryArgProfileIsUnknown (\s a -> s {_qapcForwardWhenQueryArgProfileIsUnknown = a})

instance FromXML QueryArgProfileConfig where
  parseXML x =
    QueryArgProfileConfig'
      <$> (x .@? "QueryArgProfiles")
      <*> (x .@ "ForwardWhenQueryArgProfileIsUnknown")

instance Hashable QueryArgProfileConfig

instance NFData QueryArgProfileConfig

instance ToXML QueryArgProfileConfig where
  toXML QueryArgProfileConfig' {..} =
    mconcat
      [ "QueryArgProfiles" @= _qapcQueryArgProfiles,
        "ForwardWhenQueryArgProfileIsUnknown"
          @= _qapcForwardWhenQueryArgProfileIsUnknown
      ]
