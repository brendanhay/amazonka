{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Failure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Failure where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A failed resource. For a list of common causes, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'failure' smart constructor.
data Failure = Failure'
  { _fArn :: !(Maybe Text),
    _fReason :: !(Maybe Text),
    _fDetail :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Failure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fArn' - The Amazon Resource Name (ARN) of the failed resource.
--
-- * 'fReason' - The reason for the failure.
--
-- * 'fDetail' - The details of the failure.
failure ::
  Failure
failure =
  Failure' {_fArn = Nothing, _fReason = Nothing, _fDetail = Nothing}

-- | The Amazon Resource Name (ARN) of the failed resource.
fArn :: Lens' Failure (Maybe Text)
fArn = lens _fArn (\s a -> s {_fArn = a})

-- | The reason for the failure.
fReason :: Lens' Failure (Maybe Text)
fReason = lens _fReason (\s a -> s {_fReason = a})

-- | The details of the failure.
fDetail :: Lens' Failure (Maybe Text)
fDetail = lens _fDetail (\s a -> s {_fDetail = a})

instance FromJSON Failure where
  parseJSON =
    withObject
      "Failure"
      ( \x ->
          Failure'
            <$> (x .:? "arn") <*> (x .:? "reason") <*> (x .:? "detail")
      )

instance Hashable Failure

instance NFData Failure
