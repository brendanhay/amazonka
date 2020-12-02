{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTrigger where

import Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a trigger for a repository.
--
--
--
-- /See:/ 'repositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { _rtBranches ::
      !(Maybe [Text]),
    _rtCustomData :: !(Maybe Text),
    _rtName :: !Text,
    _rtDestinationARN :: !Text,
    _rtEvents :: ![RepositoryTriggerEventEnum]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RepositoryTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtBranches' - The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
--
-- * 'rtCustomData' - Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
--
-- * 'rtName' - The name of the trigger.
--
-- * 'rtDestinationARN' - The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
--
-- * 'rtEvents' - The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS.
repositoryTrigger ::
  -- | 'rtName'
  Text ->
  -- | 'rtDestinationARN'
  Text ->
  RepositoryTrigger
repositoryTrigger pName_ pDestinationARN_ =
  RepositoryTrigger'
    { _rtBranches = Nothing,
      _rtCustomData = Nothing,
      _rtName = pName_,
      _rtDestinationARN = pDestinationARN_,
      _rtEvents = mempty
    }

-- | The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
rtBranches :: Lens' RepositoryTrigger [Text]
rtBranches = lens _rtBranches (\s a -> s {_rtBranches = a}) . _Default . _Coerce

-- | Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
rtCustomData :: Lens' RepositoryTrigger (Maybe Text)
rtCustomData = lens _rtCustomData (\s a -> s {_rtCustomData = a})

-- | The name of the trigger.
rtName :: Lens' RepositoryTrigger Text
rtName = lens _rtName (\s a -> s {_rtName = a})

-- | The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
rtDestinationARN :: Lens' RepositoryTrigger Text
rtDestinationARN = lens _rtDestinationARN (\s a -> s {_rtDestinationARN = a})

-- | The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS.
rtEvents :: Lens' RepositoryTrigger [RepositoryTriggerEventEnum]
rtEvents = lens _rtEvents (\s a -> s {_rtEvents = a}) . _Coerce

instance FromJSON RepositoryTrigger where
  parseJSON =
    withObject
      "RepositoryTrigger"
      ( \x ->
          RepositoryTrigger'
            <$> (x .:? "branches" .!= mempty)
            <*> (x .:? "customData")
            <*> (x .: "name")
            <*> (x .: "destinationArn")
            <*> (x .:? "events" .!= mempty)
      )

instance Hashable RepositoryTrigger

instance NFData RepositoryTrigger

instance ToJSON RepositoryTrigger where
  toJSON RepositoryTrigger' {..} =
    object
      ( catMaybes
          [ ("branches" .=) <$> _rtBranches,
            ("customData" .=) <$> _rtCustomData,
            Just ("name" .= _rtName),
            Just ("destinationArn" .= _rtDestinationARN),
            Just ("events" .= _rtEvents)
          ]
      )
