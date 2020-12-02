{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopRequestSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopRequestSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource

-- | Container for configuring the source of human task requests.
--
--
--
-- /See:/ 'humanLoopRequestSource' smart constructor.
newtype HumanLoopRequestSource = HumanLoopRequestSource'
  { _hlrsAWSManagedHumanLoopRequestSource ::
      AWSManagedHumanLoopRequestSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopRequestSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlrsAWSManagedHumanLoopRequestSource' - Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
humanLoopRequestSource ::
  -- | 'hlrsAWSManagedHumanLoopRequestSource'
  AWSManagedHumanLoopRequestSource ->
  HumanLoopRequestSource
humanLoopRequestSource pAWSManagedHumanLoopRequestSource_ =
  HumanLoopRequestSource'
    { _hlrsAWSManagedHumanLoopRequestSource =
        pAWSManagedHumanLoopRequestSource_
    }

-- | Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
hlrsAWSManagedHumanLoopRequestSource :: Lens' HumanLoopRequestSource AWSManagedHumanLoopRequestSource
hlrsAWSManagedHumanLoopRequestSource = lens _hlrsAWSManagedHumanLoopRequestSource (\s a -> s {_hlrsAWSManagedHumanLoopRequestSource = a})

instance FromJSON HumanLoopRequestSource where
  parseJSON =
    withObject
      "HumanLoopRequestSource"
      ( \x ->
          HumanLoopRequestSource'
            <$> (x .: "AwsManagedHumanLoopRequestSource")
      )

instance Hashable HumanLoopRequestSource

instance NFData HumanLoopRequestSource

instance ToJSON HumanLoopRequestSource where
  toJSON HumanLoopRequestSource' {..} =
    object
      ( catMaybes
          [ Just
              ( "AwsManagedHumanLoopRequestSource"
                  .= _hlrsAWSManagedHumanLoopRequestSource
              )
          ]
      )
