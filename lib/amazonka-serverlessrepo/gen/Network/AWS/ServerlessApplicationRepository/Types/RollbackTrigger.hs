{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
--
--
-- /See:/ 'rollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { _rtType :: !Text,
    _rtARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtType' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- * 'rtARN' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rollbackTrigger ::
  -- | 'rtType'
  Text ->
  -- | 'rtARN'
  Text ->
  RollbackTrigger
rollbackTrigger pType_ pARN_ =
  RollbackTrigger' {_rtType = pType_, _rtARN = pARN_}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rtType :: Lens' RollbackTrigger Text
rtType = lens _rtType (\s a -> s {_rtType = a})

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
rtARN :: Lens' RollbackTrigger Text
rtARN = lens _rtARN (\s a -> s {_rtARN = a})

instance Hashable RollbackTrigger

instance NFData RollbackTrigger

instance ToJSON RollbackTrigger where
  toJSON RollbackTrigger' {..} =
    object
      (catMaybes [Just ("type" .= _rtType), Just ("arn" .= _rtARN)])
