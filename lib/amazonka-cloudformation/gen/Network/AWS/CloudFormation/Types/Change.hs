{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Change where

import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.ResourceChange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Change@ structure describes the changes AWS CloudFormation will perform if you execute the change set.
--
--
--
-- /See:/ 'change' smart constructor.
data Change = Change'
  { _cResourceChange :: !(Maybe ResourceChange),
    _cType :: !(Maybe ChangeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResourceChange' - A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
--
-- * 'cType' - The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
change ::
  Change
change = Change' {_cResourceChange = Nothing, _cType = Nothing}

-- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
cResourceChange :: Lens' Change (Maybe ResourceChange)
cResourceChange = lens _cResourceChange (\s a -> s {_cResourceChange = a})

-- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
cType :: Lens' Change (Maybe ChangeType)
cType = lens _cType (\s a -> s {_cType = a})

instance FromXML Change where
  parseXML x =
    Change' <$> (x .@? "ResourceChange") <*> (x .@? "Type")

instance Hashable Change

instance NFData Change
