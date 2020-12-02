{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareTarget where

import Network.AWS.DirectoryService.Types.TargetType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifier that contains details about the directory consumer account.
--
--
--
-- /See:/ 'shareTarget' smart constructor.
data ShareTarget = ShareTarget'
  { _stId :: !Text,
    _stType :: !TargetType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShareTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stId' - Identifier of the directory consumer account.
--
-- * 'stType' - Type of identifier to be used in the @Id@ field.
shareTarget ::
  -- | 'stId'
  Text ->
  -- | 'stType'
  TargetType ->
  ShareTarget
shareTarget pId_ pType_ =
  ShareTarget' {_stId = pId_, _stType = pType_}

-- | Identifier of the directory consumer account.
stId :: Lens' ShareTarget Text
stId = lens _stId (\s a -> s {_stId = a})

-- | Type of identifier to be used in the @Id@ field.
stType :: Lens' ShareTarget TargetType
stType = lens _stType (\s a -> s {_stType = a})

instance Hashable ShareTarget

instance NFData ShareTarget

instance ToJSON ShareTarget where
  toJSON ShareTarget' {..} =
    object
      (catMaybes [Just ("Id" .= _stId), Just ("Type" .= _stType)])
