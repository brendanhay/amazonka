{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ErrorDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the reason that the operation failed.
--
--
-- This data type is used as a response element in the 'GetOrganizationsAccessReport' , 'GetServiceLastAccessedDetails' , and 'GetServiceLastAccessedDetailsWithEntities' operations.
--
--
-- /See:/ 'errorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { _edMessage :: !Text,
    _edCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edMessage' - Detailed information about the reason that the operation failed.
--
-- * 'edCode' - The error code associated with the operation failure.
errorDetails ::
  -- | 'edMessage'
  Text ->
  -- | 'edCode'
  Text ->
  ErrorDetails
errorDetails pMessage_ pCode_ =
  ErrorDetails' {_edMessage = pMessage_, _edCode = pCode_}

-- | Detailed information about the reason that the operation failed.
edMessage :: Lens' ErrorDetails Text
edMessage = lens _edMessage (\s a -> s {_edMessage = a})

-- | The error code associated with the operation failure.
edCode :: Lens' ErrorDetails Text
edCode = lens _edCode (\s a -> s {_edCode = a})

instance FromXML ErrorDetails where
  parseXML x = ErrorDetails' <$> (x .@ "Message") <*> (x .@ "Code")

instance Hashable ErrorDetails

instance NFData ErrorDetails
