{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DestinationDescription where

import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the destination for a delivery stream.
--
--
--
-- /See:/ 'destinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { _ddSplunkDestinationDescription ::
      !(Maybe SplunkDestinationDescription),
    _ddHTTPEndpointDestinationDescription ::
      !(Maybe HTTPEndpointDestinationDescription),
    _ddS3DestinationDescription ::
      !(Maybe S3DestinationDescription),
    _ddExtendedS3DestinationDescription ::
      !(Maybe ExtendedS3DestinationDescription),
    _ddElasticsearchDestinationDescription ::
      !(Maybe ElasticsearchDestinationDescription),
    _ddRedshiftDestinationDescription ::
      !(Maybe RedshiftDestinationDescription),
    _ddDestinationId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddSplunkDestinationDescription' - The destination in Splunk.
--
-- * 'ddHTTPEndpointDestinationDescription' - Describes the specified HTTP endpoint destination.
--
-- * 'ddS3DestinationDescription' - [Deprecated] The destination in Amazon S3.
--
-- * 'ddExtendedS3DestinationDescription' - The destination in Amazon S3.
--
-- * 'ddElasticsearchDestinationDescription' - The destination in Amazon ES.
--
-- * 'ddRedshiftDestinationDescription' - The destination in Amazon Redshift.
--
-- * 'ddDestinationId' - The ID of the destination.
destinationDescription ::
  -- | 'ddDestinationId'
  Text ->
  DestinationDescription
destinationDescription pDestinationId_ =
  DestinationDescription'
    { _ddSplunkDestinationDescription =
        Nothing,
      _ddHTTPEndpointDestinationDescription = Nothing,
      _ddS3DestinationDescription = Nothing,
      _ddExtendedS3DestinationDescription = Nothing,
      _ddElasticsearchDestinationDescription = Nothing,
      _ddRedshiftDestinationDescription = Nothing,
      _ddDestinationId = pDestinationId_
    }

-- | The destination in Splunk.
ddSplunkDestinationDescription :: Lens' DestinationDescription (Maybe SplunkDestinationDescription)
ddSplunkDestinationDescription = lens _ddSplunkDestinationDescription (\s a -> s {_ddSplunkDestinationDescription = a})

-- | Describes the specified HTTP endpoint destination.
ddHTTPEndpointDestinationDescription :: Lens' DestinationDescription (Maybe HTTPEndpointDestinationDescription)
ddHTTPEndpointDestinationDescription = lens _ddHTTPEndpointDestinationDescription (\s a -> s {_ddHTTPEndpointDestinationDescription = a})

-- | [Deprecated] The destination in Amazon S3.
ddS3DestinationDescription :: Lens' DestinationDescription (Maybe S3DestinationDescription)
ddS3DestinationDescription = lens _ddS3DestinationDescription (\s a -> s {_ddS3DestinationDescription = a})

-- | The destination in Amazon S3.
ddExtendedS3DestinationDescription :: Lens' DestinationDescription (Maybe ExtendedS3DestinationDescription)
ddExtendedS3DestinationDescription = lens _ddExtendedS3DestinationDescription (\s a -> s {_ddExtendedS3DestinationDescription = a})

-- | The destination in Amazon ES.
ddElasticsearchDestinationDescription :: Lens' DestinationDescription (Maybe ElasticsearchDestinationDescription)
ddElasticsearchDestinationDescription = lens _ddElasticsearchDestinationDescription (\s a -> s {_ddElasticsearchDestinationDescription = a})

-- | The destination in Amazon Redshift.
ddRedshiftDestinationDescription :: Lens' DestinationDescription (Maybe RedshiftDestinationDescription)
ddRedshiftDestinationDescription = lens _ddRedshiftDestinationDescription (\s a -> s {_ddRedshiftDestinationDescription = a})

-- | The ID of the destination.
ddDestinationId :: Lens' DestinationDescription Text
ddDestinationId = lens _ddDestinationId (\s a -> s {_ddDestinationId = a})

instance FromJSON DestinationDescription where
  parseJSON =
    withObject
      "DestinationDescription"
      ( \x ->
          DestinationDescription'
            <$> (x .:? "SplunkDestinationDescription")
            <*> (x .:? "HttpEndpointDestinationDescription")
            <*> (x .:? "S3DestinationDescription")
            <*> (x .:? "ExtendedS3DestinationDescription")
            <*> (x .:? "ElasticsearchDestinationDescription")
            <*> (x .:? "RedshiftDestinationDescription")
            <*> (x .: "DestinationId")
      )

instance Hashable DestinationDescription

instance NFData DestinationDescription
