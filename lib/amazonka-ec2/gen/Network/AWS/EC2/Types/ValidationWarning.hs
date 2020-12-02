{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationWarning where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ValidationError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The error codes and error messages that are returned for the parameters or parameter combinations that are not valid when a new launch template or new version of a launch template is created.
--
--
--
-- /See:/ 'validationWarning' smart constructor.
newtype ValidationWarning = ValidationWarning'
  { _vwErrors ::
      Maybe [ValidationError]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vwErrors' - The error codes and error messages.
validationWarning ::
  ValidationWarning
validationWarning = ValidationWarning' {_vwErrors = Nothing}

-- | The error codes and error messages.
vwErrors :: Lens' ValidationWarning [ValidationError]
vwErrors = lens _vwErrors (\s a -> s {_vwErrors = a}) . _Default . _Coerce

instance FromXML ValidationWarning where
  parseXML x =
    ValidationWarning'
      <$> (x .@? "errorSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ValidationWarning

instance NFData ValidationWarning
