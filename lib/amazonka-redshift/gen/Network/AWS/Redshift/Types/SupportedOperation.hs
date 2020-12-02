{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedOperation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes the operations that are allowed on a maintenance track.
--
--
--
-- /See:/ 'supportedOperation' smart constructor.
newtype SupportedOperation = SupportedOperation'
  { _soOperationName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SupportedOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soOperationName' - A list of the supported operations.
supportedOperation ::
  SupportedOperation
supportedOperation =
  SupportedOperation' {_soOperationName = Nothing}

-- | A list of the supported operations.
soOperationName :: Lens' SupportedOperation (Maybe Text)
soOperationName = lens _soOperationName (\s a -> s {_soOperationName = a})

instance FromXML SupportedOperation where
  parseXML x = SupportedOperation' <$> (x .@? "OperationName")

instance Hashable SupportedOperation

instance NFData SupportedOperation
