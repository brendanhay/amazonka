{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Delegate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Delegate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.MemberType

-- | The name of the attribute, which is one of the values defined in the UserAttribute enumeration.
--
--
--
-- /See:/ 'delegate' smart constructor.
data Delegate = Delegate' {_dId :: !Text, _dType :: !MemberType}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Delegate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The identifier for the user or group associated as the resource's delegate.
--
-- * 'dType' - The type of the delegate: user or group.
delegate ::
  -- | 'dId'
  Text ->
  -- | 'dType'
  MemberType ->
  Delegate
delegate pId_ pType_ = Delegate' {_dId = pId_, _dType = pType_}

-- | The identifier for the user or group associated as the resource's delegate.
dId :: Lens' Delegate Text
dId = lens _dId (\s a -> s {_dId = a})

-- | The type of the delegate: user or group.
dType :: Lens' Delegate MemberType
dType = lens _dType (\s a -> s {_dType = a})

instance FromJSON Delegate where
  parseJSON =
    withObject
      "Delegate"
      (\x -> Delegate' <$> (x .: "Id") <*> (x .: "Type"))

instance Hashable Delegate

instance NFData Delegate
