{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.UnshareTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.UnshareTarget where

import Network.AWS.DirectoryService.Types.TargetType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifier that contains details about the directory consumer account with whom the directory is being unshared.
--
--
--
-- /See:/ 'unshareTarget' smart constructor.
data UnshareTarget = UnshareTarget'
  { _utId :: !Text,
    _utType :: !TargetType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnshareTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utId' - Identifier of the directory consumer account.
--
-- * 'utType' - Type of identifier to be used in the /Id/ field.
unshareTarget ::
  -- | 'utId'
  Text ->
  -- | 'utType'
  TargetType ->
  UnshareTarget
unshareTarget pId_ pType_ =
  UnshareTarget' {_utId = pId_, _utType = pType_}

-- | Identifier of the directory consumer account.
utId :: Lens' UnshareTarget Text
utId = lens _utId (\s a -> s {_utId = a})

-- | Type of identifier to be used in the /Id/ field.
utType :: Lens' UnshareTarget TargetType
utType = lens _utType (\s a -> s {_utType = a})

instance Hashable UnshareTarget

instance NFData UnshareTarget

instance ToJSON UnshareTarget where
  toJSON UnshareTarget' {..} =
    object
      (catMaybes [Just ("Id" .= _utId), Just ("Type" .= _utType)])
