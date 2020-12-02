{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about a platform branch.
--
--
--
-- /See:/ 'platformBranchSummary' smart constructor.
data PlatformBranchSummary = PlatformBranchSummary'
  { _pbsBranchName ::
      !(Maybe Text),
    _pbsBranchOrder :: !(Maybe Int),
    _pbsPlatformName :: !(Maybe Text),
    _pbsSupportedTierList :: !(Maybe [Text]),
    _pbsLifecycleState :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlatformBranchSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbsBranchName' - The name of the platform branch.
--
-- * 'pbsBranchOrder' - An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches. A larger @BranchOrder@ value designates a newer platform branch within the platform.
--
-- * 'pbsPlatformName' - The name of the platform to which this platform branch belongs.
--
-- * 'pbsSupportedTierList' - The environment tiers that platform versions in this branch support. Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@
--
-- * 'pbsLifecycleState' - The support life cycle state of the platform branch. Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
platformBranchSummary ::
  PlatformBranchSummary
platformBranchSummary =
  PlatformBranchSummary'
    { _pbsBranchName = Nothing,
      _pbsBranchOrder = Nothing,
      _pbsPlatformName = Nothing,
      _pbsSupportedTierList = Nothing,
      _pbsLifecycleState = Nothing
    }

-- | The name of the platform branch.
pbsBranchName :: Lens' PlatformBranchSummary (Maybe Text)
pbsBranchName = lens _pbsBranchName (\s a -> s {_pbsBranchName = a})

-- | An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches. A larger @BranchOrder@ value designates a newer platform branch within the platform.
pbsBranchOrder :: Lens' PlatformBranchSummary (Maybe Int)
pbsBranchOrder = lens _pbsBranchOrder (\s a -> s {_pbsBranchOrder = a})

-- | The name of the platform to which this platform branch belongs.
pbsPlatformName :: Lens' PlatformBranchSummary (Maybe Text)
pbsPlatformName = lens _pbsPlatformName (\s a -> s {_pbsPlatformName = a})

-- | The environment tiers that platform versions in this branch support. Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@
pbsSupportedTierList :: Lens' PlatformBranchSummary [Text]
pbsSupportedTierList = lens _pbsSupportedTierList (\s a -> s {_pbsSupportedTierList = a}) . _Default . _Coerce

-- | The support life cycle state of the platform branch. Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
pbsLifecycleState :: Lens' PlatformBranchSummary (Maybe Text)
pbsLifecycleState = lens _pbsLifecycleState (\s a -> s {_pbsLifecycleState = a})

instance FromXML PlatformBranchSummary where
  parseXML x =
    PlatformBranchSummary'
      <$> (x .@? "BranchName")
      <*> (x .@? "BranchOrder")
      <*> (x .@? "PlatformName")
      <*> ( x .@? "SupportedTierList" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "LifecycleState")

instance Hashable PlatformBranchSummary

instance NFData PlatformBranchSummary
