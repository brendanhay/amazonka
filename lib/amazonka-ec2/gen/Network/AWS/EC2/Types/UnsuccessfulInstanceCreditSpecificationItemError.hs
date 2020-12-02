{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the error for the burstable performance instance whose credit option for CPU usage was not modified.
--
--
--
-- /See:/ 'unsuccessfulInstanceCreditSpecificationItemError' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItemError = UnsuccessfulInstanceCreditSpecificationItemError'
  { _uicsieCode ::
      !( Maybe
           UnsuccessfulInstanceCreditSpecificationErrorCode
       ),
    _uicsieMessage ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItemError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicsieCode' - The error code.
--
-- * 'uicsieMessage' - The applicable error message.
unsuccessfulInstanceCreditSpecificationItemError ::
  UnsuccessfulInstanceCreditSpecificationItemError
unsuccessfulInstanceCreditSpecificationItemError =
  UnsuccessfulInstanceCreditSpecificationItemError'
    { _uicsieCode =
        Nothing,
      _uicsieMessage = Nothing
    }

-- | The error code.
uicsieCode :: Lens' UnsuccessfulInstanceCreditSpecificationItemError (Maybe UnsuccessfulInstanceCreditSpecificationErrorCode)
uicsieCode = lens _uicsieCode (\s a -> s {_uicsieCode = a})

-- | The applicable error message.
uicsieMessage :: Lens' UnsuccessfulInstanceCreditSpecificationItemError (Maybe Text)
uicsieMessage = lens _uicsieMessage (\s a -> s {_uicsieMessage = a})

instance FromXML UnsuccessfulInstanceCreditSpecificationItemError where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItemError'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable UnsuccessfulInstanceCreditSpecificationItemError

instance NFData UnsuccessfulInstanceCreditSpecificationItemError
