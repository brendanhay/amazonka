{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.SourceSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.SourceSecurityGroup where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a source security group.
--
--
--
-- /See:/ 'sourceSecurityGroup' smart constructor.
data SourceSecurityGroup = SourceSecurityGroup'
  { _ssgOwnerAlias ::
      !(Maybe Text),
    _ssgGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgOwnerAlias' - The owner of the security group.
--
-- * 'ssgGroupName' - The name of the security group.
sourceSecurityGroup ::
  SourceSecurityGroup
sourceSecurityGroup =
  SourceSecurityGroup'
    { _ssgOwnerAlias = Nothing,
      _ssgGroupName = Nothing
    }

-- | The owner of the security group.
ssgOwnerAlias :: Lens' SourceSecurityGroup (Maybe Text)
ssgOwnerAlias = lens _ssgOwnerAlias (\s a -> s {_ssgOwnerAlias = a})

-- | The name of the security group.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\s a -> s {_ssgGroupName = a})

instance FromXML SourceSecurityGroup where
  parseXML x =
    SourceSecurityGroup'
      <$> (x .@? "OwnerAlias") <*> (x .@? "GroupName")

instance Hashable SourceSecurityGroup

instance NFData SourceSecurityGroup
