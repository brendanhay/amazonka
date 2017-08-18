{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.PublishSchema
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a development schema with a version. If description and attributes are specified, @PublishSchema@ overrides the development schema description and attributes. If not, the development schema description and attributes are used.
--
--
module Network.AWS.CloudDirectory.PublishSchema
    (
    -- * Creating a Request
      publishSchema
    , PublishSchema
    -- * Request Lenses
    , psName
    , psDevelopmentSchemaARN
    , psVersion

    -- * Destructuring the Response
    , publishSchemaResponse
    , PublishSchemaResponse
    -- * Response Lenses
    , psrsPublishedSchemaARN
    , psrsResponseStatus
    ) where

import           Network.AWS.CloudDirectory.Types
import           Network.AWS.CloudDirectory.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'publishSchema' smart constructor.
data PublishSchema = PublishSchema'
    { _psName                 :: !(Maybe Text)
    , _psDevelopmentSchemaARN :: !Text
    , _psVersion              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublishSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psName' - The new name under which the schema will be published. If this is not provided, the development schema is considered.
--
-- * 'psDevelopmentSchemaARN' - The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
--
-- * 'psVersion' - The version under which the schema will be published.
publishSchema
    :: Text -- ^ 'psDevelopmentSchemaARN'
    -> Text -- ^ 'psVersion'
    -> PublishSchema
publishSchema pDevelopmentSchemaARN_ pVersion_ =
    PublishSchema'
    { _psName = Nothing
    , _psDevelopmentSchemaARN = pDevelopmentSchemaARN_
    , _psVersion = pVersion_
    }

-- | The new name under which the schema will be published. If this is not provided, the development schema is considered.
psName :: Lens' PublishSchema (Maybe Text)
psName = lens _psName (\ s a -> s{_psName = a});

-- | The Amazon Resource Name (ARN) that is associated with the development schema. For more information, see 'arns' .
psDevelopmentSchemaARN :: Lens' PublishSchema Text
psDevelopmentSchemaARN = lens _psDevelopmentSchemaARN (\ s a -> s{_psDevelopmentSchemaARN = a});

-- | The version under which the schema will be published.
psVersion :: Lens' PublishSchema Text
psVersion = lens _psVersion (\ s a -> s{_psVersion = a});

instance AWSRequest PublishSchema where
        type Rs PublishSchema = PublishSchemaResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 PublishSchemaResponse' <$>
                   (x .?> "PublishedSchemaArn") <*> (pure (fromEnum s)))

instance Hashable PublishSchema

instance NFData PublishSchema

instance ToHeaders PublishSchema where
        toHeaders PublishSchema'{..}
          = mconcat
              ["x-amz-data-partition" =# _psDevelopmentSchemaARN]

instance ToJSON PublishSchema where
        toJSON PublishSchema'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _psName,
                  Just ("Version" .= _psVersion)])

instance ToPath PublishSchema where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/publish"

instance ToQuery PublishSchema where
        toQuery = const mempty

-- | /See:/ 'publishSchemaResponse' smart constructor.
data PublishSchemaResponse = PublishSchemaResponse'
    { _psrsPublishedSchemaARN :: !(Maybe Text)
    , _psrsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublishSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psrsPublishedSchemaARN' - The ARN that is associated with the published schema. For more information, see 'arns' .
--
-- * 'psrsResponseStatus' - -- | The response status code.
publishSchemaResponse
    :: Int -- ^ 'psrsResponseStatus'
    -> PublishSchemaResponse
publishSchemaResponse pResponseStatus_ =
    PublishSchemaResponse'
    { _psrsPublishedSchemaARN = Nothing
    , _psrsResponseStatus = pResponseStatus_
    }

-- | The ARN that is associated with the published schema. For more information, see 'arns' .
psrsPublishedSchemaARN :: Lens' PublishSchemaResponse (Maybe Text)
psrsPublishedSchemaARN = lens _psrsPublishedSchemaARN (\ s a -> s{_psrsPublishedSchemaARN = a});

-- | -- | The response status code.
psrsResponseStatus :: Lens' PublishSchemaResponse Int
psrsResponseStatus = lens _psrsResponseStatus (\ s a -> s{_psrsResponseStatus = a});

instance NFData PublishSchemaResponse
