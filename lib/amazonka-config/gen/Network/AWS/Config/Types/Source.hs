{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Source where

import Network.AWS.Config.Types.Owner
import Network.AWS.Config.Types.SourceDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.
--
--
--
-- /See:/ 'source' smart constructor.
data Source = Source'
  { _sSourceDetails :: !(Maybe [SourceDetail]),
    _sOwner :: !Owner,
    _sSourceIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSourceDetails' - Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
--
-- * 'sOwner' - Indicates whether AWS or the customer owns and manages the AWS Config rule.
--
-- * 'sSourceIdentifier' - For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> . For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
source ::
  -- | 'sOwner'
  Owner ->
  -- | 'sSourceIdentifier'
  Text ->
  Source
source pOwner_ pSourceIdentifier_ =
  Source'
    { _sSourceDetails = Nothing,
      _sOwner = pOwner_,
      _sSourceIdentifier = pSourceIdentifier_
    }

-- | Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
sSourceDetails :: Lens' Source [SourceDetail]
sSourceDetails = lens _sSourceDetails (\s a -> s {_sSourceDetails = a}) . _Default . _Coerce

-- | Indicates whether AWS or the customer owns and manages the AWS Config rule.
sOwner :: Lens' Source Owner
sOwner = lens _sOwner (\s a -> s {_sOwner = a})

-- | For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> . For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
sSourceIdentifier :: Lens' Source Text
sSourceIdentifier = lens _sSourceIdentifier (\s a -> s {_sSourceIdentifier = a})

instance FromJSON Source where
  parseJSON =
    withObject
      "Source"
      ( \x ->
          Source'
            <$> (x .:? "SourceDetails" .!= mempty)
            <*> (x .: "Owner")
            <*> (x .: "SourceIdentifier")
      )

instance Hashable Source

instance NFData Source

instance ToJSON Source where
  toJSON Source' {..} =
    object
      ( catMaybes
          [ ("SourceDetails" .=) <$> _sSourceDetails,
            Just ("Owner" .= _sOwner),
            Just ("SourceIdentifier" .= _sSourceIdentifier)
          ]
      )
