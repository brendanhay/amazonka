{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJobDetails where

import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a job sent in response to a @GetThirdPartyJobDetails@ request.
--
--
--
-- /See:/ 'thirdPartyJobDetails' smart constructor.
data ThirdPartyJobDetails = ThirdPartyJobDetails'
  { _tpjdData ::
      !(Maybe ThirdPartyJobData),
    _tpjdId :: !(Maybe Text),
    _tpjdNonce :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThirdPartyJobDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpjdData' - The data to be returned by the third party job worker.
--
-- * 'tpjdId' - The identifier used to identify the job details in AWS CodePipeline.
--
-- * 'tpjdNonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
thirdPartyJobDetails ::
  ThirdPartyJobDetails
thirdPartyJobDetails =
  ThirdPartyJobDetails'
    { _tpjdData = Nothing,
      _tpjdId = Nothing,
      _tpjdNonce = Nothing
    }

-- | The data to be returned by the third party job worker.
tpjdData :: Lens' ThirdPartyJobDetails (Maybe ThirdPartyJobData)
tpjdData = lens _tpjdData (\s a -> s {_tpjdData = a})

-- | The identifier used to identify the job details in AWS CodePipeline.
tpjdId :: Lens' ThirdPartyJobDetails (Maybe Text)
tpjdId = lens _tpjdId (\s a -> s {_tpjdId = a})

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
tpjdNonce :: Lens' ThirdPartyJobDetails (Maybe Text)
tpjdNonce = lens _tpjdNonce (\s a -> s {_tpjdNonce = a})

instance FromJSON ThirdPartyJobDetails where
  parseJSON =
    withObject
      "ThirdPartyJobDetails"
      ( \x ->
          ThirdPartyJobDetails'
            <$> (x .:? "data") <*> (x .:? "id") <*> (x .:? "nonce")
      )

instance Hashable ThirdPartyJobDetails

instance NFData ThirdPartyJobDetails
