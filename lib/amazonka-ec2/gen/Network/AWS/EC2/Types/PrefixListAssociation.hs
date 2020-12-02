{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the resource with which a prefix list is associated.
--
--
--
-- /See:/ 'prefixListAssociation' smart constructor.
data PrefixListAssociation = PrefixListAssociation'
  { _plaResourceId ::
      !(Maybe Text),
    _plaResourceOwner :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrefixListAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plaResourceId' - The ID of the resource.
--
-- * 'plaResourceOwner' - The owner of the resource.
prefixListAssociation ::
  PrefixListAssociation
prefixListAssociation =
  PrefixListAssociation'
    { _plaResourceId = Nothing,
      _plaResourceOwner = Nothing
    }

-- | The ID of the resource.
plaResourceId :: Lens' PrefixListAssociation (Maybe Text)
plaResourceId = lens _plaResourceId (\s a -> s {_plaResourceId = a})

-- | The owner of the resource.
plaResourceOwner :: Lens' PrefixListAssociation (Maybe Text)
plaResourceOwner = lens _plaResourceOwner (\s a -> s {_plaResourceOwner = a})

instance FromXML PrefixListAssociation where
  parseXML x =
    PrefixListAssociation'
      <$> (x .@? "resourceId") <*> (x .@? "resourceOwner")

instance Hashable PrefixListAssociation

instance NFData PrefixListAssociation
