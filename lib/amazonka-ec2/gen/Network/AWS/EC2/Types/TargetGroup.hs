{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroup where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a load balancer target group.
--
--
--
-- /See:/ 'targetGroup' smart constructor.
newtype TargetGroup = TargetGroup' {_tgARN :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgARN' - The Amazon Resource Name (ARN) of the target group.
targetGroup ::
  TargetGroup
targetGroup = TargetGroup' {_tgARN = Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
tgARN :: Lens' TargetGroup (Maybe Text)
tgARN = lens _tgARN (\s a -> s {_tgARN = a})

instance FromXML TargetGroup where
  parseXML x = TargetGroup' <$> (x .@? "arn")

instance Hashable TargetGroup

instance NFData TargetGroup

instance ToQuery TargetGroup where
  toQuery TargetGroup' {..} = mconcat ["Arn" =: _tgARN]
