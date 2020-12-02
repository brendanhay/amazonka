{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteFleetErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 Fleet error.
--
--
--
-- /See:/ 'deleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { _dfeCode ::
      !(Maybe DeleteFleetErrorCode),
    _dfeMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeCode' - The error code.
--
-- * 'dfeMessage' - The description for the error code.
deleteFleetError ::
  DeleteFleetError
deleteFleetError =
  DeleteFleetError' {_dfeCode = Nothing, _dfeMessage = Nothing}

-- | The error code.
dfeCode :: Lens' DeleteFleetError (Maybe DeleteFleetErrorCode)
dfeCode = lens _dfeCode (\s a -> s {_dfeCode = a})

-- | The description for the error code.
dfeMessage :: Lens' DeleteFleetError (Maybe Text)
dfeMessage = lens _dfeMessage (\s a -> s {_dfeMessage = a})

instance FromXML DeleteFleetError where
  parseXML x =
    DeleteFleetError' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable DeleteFleetError

instance NFData DeleteFleetError
