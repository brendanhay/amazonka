{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryError where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.QueryErrorCode

-- | A two-part error structure that can occur in @ListGroupResources@ or @SearchResources@ operations on CloudFormation stack-based queries. The error occurs if the CloudFormation stack on which the query is based either does not exist, or has a status that renders the stack inactive. A @QueryError@ occurrence does not necessarily mean that AWS Resource Groups could not complete the operation, but the resulting group might have no member resources.
--
--
--
-- /See:/ 'queryError' smart constructor.
data QueryError = QueryError'
  { _qeErrorCode ::
      !(Maybe QueryErrorCode),
    _qeMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qeErrorCode' - Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- * 'qeMessage' - A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
queryError ::
  QueryError
queryError =
  QueryError' {_qeErrorCode = Nothing, _qeMessage = Nothing}

-- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
qeErrorCode :: Lens' QueryError (Maybe QueryErrorCode)
qeErrorCode = lens _qeErrorCode (\s a -> s {_qeErrorCode = a})

-- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
qeMessage :: Lens' QueryError (Maybe Text)
qeMessage = lens _qeMessage (\s a -> s {_qeMessage = a})

instance FromJSON QueryError where
  parseJSON =
    withObject
      "QueryError"
      (\x -> QueryError' <$> (x .:? "ErrorCode") <*> (x .:? "Message"))

instance Hashable QueryError

instance NFData QueryError
