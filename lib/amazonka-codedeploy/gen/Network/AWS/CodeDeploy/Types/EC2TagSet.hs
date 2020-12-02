{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagSet where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about groups of EC2 instance tags.
--
--
--
-- /See:/ 'ec2TagSet' smart constructor.
newtype EC2TagSet = EC2TagSet'
  { _etsEc2TagSetList ::
      Maybe [[EC2TagFilter]]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2TagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsEc2TagSetList' - A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
ec2TagSet ::
  EC2TagSet
ec2TagSet = EC2TagSet' {_etsEc2TagSetList = Nothing}

-- | A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
etsEc2TagSetList :: Lens' EC2TagSet [[EC2TagFilter]]
etsEc2TagSetList = lens _etsEc2TagSetList (\s a -> s {_etsEc2TagSetList = a}) . _Default . _Coerce

instance FromJSON EC2TagSet where
  parseJSON =
    withObject
      "EC2TagSet"
      (\x -> EC2TagSet' <$> (x .:? "ec2TagSetList" .!= mempty))

instance Hashable EC2TagSet

instance NFData EC2TagSet

instance ToJSON EC2TagSet where
  toJSON EC2TagSet' {..} =
    object (catMaybes [("ec2TagSetList" .=) <$> _etsEc2TagSetList])
