{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItemError where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the error that occurred. For more information about errors, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
--
--
--
-- /See:/ 'unsuccessfulItemError' smart constructor.
data UnsuccessfulItemError = UnsuccessfulItemError'
  { _uieCode ::
      !(Maybe Text),
    _uieMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnsuccessfulItemError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uieCode' - The error code.
--
-- * 'uieMessage' - The error message accompanying the error code.
unsuccessfulItemError ::
  UnsuccessfulItemError
unsuccessfulItemError =
  UnsuccessfulItemError' {_uieCode = Nothing, _uieMessage = Nothing}

-- | The error code.
uieCode :: Lens' UnsuccessfulItemError (Maybe Text)
uieCode = lens _uieCode (\s a -> s {_uieCode = a})

-- | The error message accompanying the error code.
uieMessage :: Lens' UnsuccessfulItemError (Maybe Text)
uieMessage = lens _uieMessage (\s a -> s {_uieMessage = a})

instance FromXML UnsuccessfulItemError where
  parseXML x =
    UnsuccessfulItemError' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable UnsuccessfulItemError

instance NFData UnsuccessfulItemError
