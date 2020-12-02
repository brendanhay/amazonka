{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the error for a Reserved Instance whose queued purchase could not be deleted.
--
--
--
-- /See:/ 'deleteQueuedReservedInstancesError' smart constructor.
data DeleteQueuedReservedInstancesError = DeleteQueuedReservedInstancesError'
  { _dqrieCode ::
      !( Maybe
           DeleteQueuedReservedInstancesErrorCode
       ),
    _dqrieMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteQueuedReservedInstancesError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqrieCode' - The error code.
--
-- * 'dqrieMessage' - The error message.
deleteQueuedReservedInstancesError ::
  DeleteQueuedReservedInstancesError
deleteQueuedReservedInstancesError =
  DeleteQueuedReservedInstancesError'
    { _dqrieCode = Nothing,
      _dqrieMessage = Nothing
    }

-- | The error code.
dqrieCode :: Lens' DeleteQueuedReservedInstancesError (Maybe DeleteQueuedReservedInstancesErrorCode)
dqrieCode = lens _dqrieCode (\s a -> s {_dqrieCode = a})

-- | The error message.
dqrieMessage :: Lens' DeleteQueuedReservedInstancesError (Maybe Text)
dqrieMessage = lens _dqrieMessage (\s a -> s {_dqrieMessage = a})

instance FromXML DeleteQueuedReservedInstancesError where
  parseXML x =
    DeleteQueuedReservedInstancesError'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable DeleteQueuedReservedInstancesError

instance NFData DeleteQueuedReservedInstancesError
