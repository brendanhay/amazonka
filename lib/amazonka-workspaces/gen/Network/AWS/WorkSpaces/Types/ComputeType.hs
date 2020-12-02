{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ComputeType where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.Compute

-- | Describes the compute type.
--
--
--
-- /See:/ 'computeType' smart constructor.
newtype ComputeType = ComputeType' {_ctName :: Maybe Compute}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctName' - The compute type.
computeType ::
  ComputeType
computeType = ComputeType' {_ctName = Nothing}

-- | The compute type.
ctName :: Lens' ComputeType (Maybe Compute)
ctName = lens _ctName (\s a -> s {_ctName = a})

instance FromJSON ComputeType where
  parseJSON =
    withObject "ComputeType" (\x -> ComputeType' <$> (x .:? "Name"))

instance Hashable ComputeType

instance NFData ComputeType
