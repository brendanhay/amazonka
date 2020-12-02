{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJob where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A response to a @PollForThirdPartyJobs@ request returned by AWS CodePipeline when there is a job to be worked on by a partner action.
--
--
--
-- /See:/ 'thirdPartyJob' smart constructor.
data ThirdPartyJob = ThirdPartyJob'
  { _tpjClientId :: !(Maybe Text),
    _tpjJobId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThirdPartyJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpjClientId' - The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
--
-- * 'tpjJobId' - The identifier used to identify the job in AWS CodePipeline.
thirdPartyJob ::
  ThirdPartyJob
thirdPartyJob =
  ThirdPartyJob' {_tpjClientId = Nothing, _tpjJobId = Nothing}

-- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
tpjClientId :: Lens' ThirdPartyJob (Maybe Text)
tpjClientId = lens _tpjClientId (\s a -> s {_tpjClientId = a})

-- | The identifier used to identify the job in AWS CodePipeline.
tpjJobId :: Lens' ThirdPartyJob (Maybe Text)
tpjJobId = lens _tpjJobId (\s a -> s {_tpjJobId = a})

instance FromJSON ThirdPartyJob where
  parseJSON =
    withObject
      "ThirdPartyJob"
      (\x -> ThirdPartyJob' <$> (x .:? "clientId") <*> (x .:? "jobId"))

instance Hashable ThirdPartyJob

instance NFData ThirdPartyJob
