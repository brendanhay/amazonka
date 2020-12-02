{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControlsRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnershipControlsRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectOwnership

-- | The container element for an ownership control rule.
--
--
--
-- /See:/ 'ownershipControlsRule' smart constructor.
newtype OwnershipControlsRule = OwnershipControlsRule'
  { _ocrObjectOwnership ::
      ObjectOwnership
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OwnershipControlsRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrObjectOwnership' - Undocumented member.
ownershipControlsRule ::
  -- | 'ocrObjectOwnership'
  ObjectOwnership ->
  OwnershipControlsRule
ownershipControlsRule pObjectOwnership_ =
  OwnershipControlsRule' {_ocrObjectOwnership = pObjectOwnership_}

-- | Undocumented member.
ocrObjectOwnership :: Lens' OwnershipControlsRule ObjectOwnership
ocrObjectOwnership = lens _ocrObjectOwnership (\s a -> s {_ocrObjectOwnership = a})

instance FromXML OwnershipControlsRule where
  parseXML x = OwnershipControlsRule' <$> (x .@ "ObjectOwnership")

instance Hashable OwnershipControlsRule

instance NFData OwnershipControlsRule

instance ToXML OwnershipControlsRule where
  toXML OwnershipControlsRule' {..} =
    mconcat ["ObjectOwnership" @= _ocrObjectOwnership]
