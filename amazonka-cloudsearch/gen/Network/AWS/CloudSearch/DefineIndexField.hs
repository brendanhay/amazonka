{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures an @IndexField@ for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the @IndexFieldType@. If the field exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html>
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Request
      DefineIndexField
    -- ** Request constructor
    , defineIndexField
    -- ** Request lenses
    , deffrqDomainName
    , deffrqIndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response constructor
    , defineIndexFieldResponse
    -- ** Response lenses
    , defrsStatus
    , defrsIndexField
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DefineIndexField@ operation.
-- Specifies the name of the domain you want to update and the index field
-- configuration.
--
-- /See:/ 'defineIndexField' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deffrqDomainName'
--
-- * 'deffrqIndexField'
data DefineIndexField = DefineIndexField'
    { _deffrqDomainName :: !Text
    , _deffrqIndexField :: !IndexField
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineIndexField' smart constructor.
defineIndexField :: Text -> IndexField -> DefineIndexField
defineIndexField pDomainName_ pIndexField_ =
    DefineIndexField'
    { _deffrqDomainName = pDomainName_
    , _deffrqIndexField = pIndexField_
    }

-- | FIXME: Undocumented member.
deffrqDomainName :: Lens' DefineIndexField Text
deffrqDomainName = lens _deffrqDomainName (\ s a -> s{_deffrqDomainName = a});

-- | The index field and field options you want to configure.
deffrqIndexField :: Lens' DefineIndexField IndexField
deffrqIndexField = lens _deffrqIndexField (\ s a -> s{_deffrqIndexField = a});

instance AWSRequest DefineIndexField where
        type Sv DefineIndexField = CloudSearch
        type Rs DefineIndexField = DefineIndexFieldResponse
        request = post
        response
          = receiveXMLWrapper "DefineIndexFieldResult"
              (\ s h x ->
                 DefineIndexFieldResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "IndexField"))

instance ToHeaders DefineIndexField where
        toHeaders = const mempty

instance ToPath DefineIndexField where
        toPath = const "/"

instance ToQuery DefineIndexField where
        toQuery DefineIndexField'{..}
          = mconcat
              ["Action" =: ("DefineIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _deffrqDomainName,
               "IndexField" =: _deffrqIndexField]

-- | The result of a @DefineIndexField@ request. Contains the status of the
-- newly-configured index field.
--
-- /See:/ 'defineIndexFieldResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defrsStatus'
--
-- * 'defrsIndexField'
data DefineIndexFieldResponse = DefineIndexFieldResponse'
    { _defrsStatus     :: !Int
    , _defrsIndexField :: !IndexFieldStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineIndexFieldResponse' smart constructor.
defineIndexFieldResponse :: Int -> IndexFieldStatus -> DefineIndexFieldResponse
defineIndexFieldResponse pStatus_ pIndexField_ =
    DefineIndexFieldResponse'
    { _defrsStatus = pStatus_
    , _defrsIndexField = pIndexField_
    }

-- | FIXME: Undocumented member.
defrsStatus :: Lens' DefineIndexFieldResponse Int
defrsStatus = lens _defrsStatus (\ s a -> s{_defrsStatus = a});

-- | FIXME: Undocumented member.
defrsIndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
defrsIndexField = lens _defrsIndexField (\ s a -> s{_defrsIndexField = a});
