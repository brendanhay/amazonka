{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Summary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains the training summary. The training summary includes aggregated evaluation metrics for the entire testing dataset and metrics for each individual label.
--
--
-- You get the training summary S3 bucket location by calling 'DescribeProjectVersions' .
--
--
-- /See:/ 'summary' smart constructor.
newtype Summary = Summary' {_sS3Object :: Maybe S3Object}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Summary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sS3Object' - Undocumented member.
summary ::
  Summary
summary = Summary' {_sS3Object = Nothing}

-- | Undocumented member.
sS3Object :: Lens' Summary (Maybe S3Object)
sS3Object = lens _sS3Object (\s a -> s {_sS3Object = a})

instance FromJSON Summary where
  parseJSON =
    withObject "Summary" (\x -> Summary' <$> (x .:? "S3Object"))

instance Hashable Summary

instance NFData Summary
