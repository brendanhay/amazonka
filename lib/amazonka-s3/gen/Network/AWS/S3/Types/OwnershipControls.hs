{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnershipControls where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.OwnershipControlsRule

-- | The container element for a bucket's ownership controls.
--
--
--
-- /See:/ 'ownershipControls' smart constructor.
newtype OwnershipControls = OwnershipControls'
  { _ocRules ::
      [OwnershipControlsRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OwnershipControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocRules' - The container element for an ownership control rule.
ownershipControls ::
  OwnershipControls
ownershipControls = OwnershipControls' {_ocRules = mempty}

-- | The container element for an ownership control rule.
ocRules :: Lens' OwnershipControls [OwnershipControlsRule]
ocRules = lens _ocRules (\s a -> s {_ocRules = a}) . _Coerce

instance FromXML OwnershipControls where
  parseXML x = OwnershipControls' <$> (parseXMLList "Rule" x)

instance Hashable OwnershipControls

instance NFData OwnershipControls

instance ToXML OwnershipControls where
  toXML OwnershipControls' {..} = mconcat [toXMLList "Rule" _ocRules]
