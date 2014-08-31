{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Import/Export accelerates moving large amounts of data into and out of
-- AWS using portable storage devices for transport. AWS transfers your data
-- directly onto and off of storage devices using Amazon’s high-speed internal
-- network and bypassing the Internet. For significant data sets, AWS
-- Import/Export is often faster than Internet transfer and more cost
-- effective than upgrading your connectivity.
module Network.AWS.ImportExport.V2010_06_01.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V2

-- | Supported version (@2010-06-01@) of the
-- @AWS Import/Export@ service.
data ImportExport deriving (Typeable)

instance AWSService ImportExport where
    type Sg ImportExport = V2
    data Er ImportExport
        = BucketPermissionException
            { _bpeMessage :: Maybe Text
            }
        | CanceledJobIdException
            { _cjieMessage :: Maybe Text
            }
        | ExpiredJobIdException
            { _ejieMessage :: Maybe Text
            }
        | ImportExportClient HttpException
        | ImportExportSerializer String
        | ImportExportService String
        | InvalidAccessKeyIdException
            { _iakieMessage :: Maybe Text
            }
        | InvalidAddressException
            { _iaeMessage :: Maybe Text
            }
        | InvalidCustomsException
            { _iceMessage :: Maybe Text
            }
        | InvalidFileSystemException
            { _ifseMessage :: Maybe Text
            }
        | InvalidJobIdException
            { _ijieMessage :: Maybe Text
            }
        | InvalidManifestFieldException
            { _imfeMessage :: Maybe Text
            }
        | InvalidParameterException
            { _ipeMessage :: Maybe Text
            }
        | MalformedManifestException
            { _mmeMessage :: Maybe Text
            }
        | MissingCustomsException
            { _mceMessage :: Maybe Text
            }
        | MissingManifestFieldException
            { _mmfeMessage :: Maybe Text
            }
        | MissingParameterException
            { _mpeMessage :: Maybe Text
            }
        | MultipleRegionsException
            { _mreMessage :: Maybe Text
            }
        | NoSuchBucketException
            { _nsbeMessage :: Maybe Text
            }
        | UnableToCancelJobIdException
            { _utcjieMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "importexport"
        , _svcVersion  = "2010-06-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ImportExport)
deriving instance Generic (Er ImportExport)

instance AWSError (Er ImportExport) where
    awsError = const "ImportExportError"

instance AWSServiceError (Er ImportExport) where
    serviceError    = ImportExportService
    clientError     = ImportExportClient
    serializerError = ImportExportSerializer

instance Exception (Er ImportExport)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://importexport.amazonaws.com/doc/2010-06-01/"
    }

-- | Specifies whether the job to initiate is an import or export job.
data JobType
    = JobTypeExport -- ^ Export
    | JobTypeImport -- ^ Import
      deriving (Eq, Show, Generic)

instance Hashable JobType

instance FromText JobType where
    parser = match "Export" JobTypeExport
         <|> match "Import" JobTypeImport

instance ToText JobType where
    toText JobTypeExport = "Export"
    toText JobTypeImport = "Import"

instance ToByteString JobType

instance FromXML JobType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "JobType"

instance ToQuery JobType where
    toQuery = genericQuery def

-- | Representation of a job returned by the ListJobs operation.
data Job = Job
    { _jJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _jJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    , _jIsCanceled :: Bool
      -- ^ Indicates whether the job was canceled.
    , _jCreationDate :: ISO8601
      -- ^ Timestamp of the CreateJob request in ISO8601 date format. For
      -- example "2010-03-28T20:27:35Z".
    } deriving (Show, Generic)

instance FromXML Job where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Job"

makeLenses ''Job
